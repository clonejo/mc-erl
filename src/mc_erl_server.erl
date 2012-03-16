-module(mc_erl_server).
-behaviour(gen_server).

-export([start_link/0, stop/0, send/2]).

-include("records.hrl").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).


% gen_server callbacks
init([]) ->
	io:format("[~s] starting~n", [?MODULE]),
	Port = 25566,
	{ok, Listen} = gen_tcp:listen(Port, [binary, {reuseaddr, true}, {active, false},
	                           {packet, raw}, {nodelay, true}]),
		spawn_link(fun() -> acceptor(Listen) end),
		{ok, Listen}.

acceptor(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			gen_server:cast(?MODULE, {new_connection, Socket}),
			acceptor(Listen);
		{error, closed} ->
			ok
	end.

handle_call(Message, _From, State) ->
	case Message of
		_ ->
			io:format("[~s] received call: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.

handle_cast({new_connection, Socket}, State) ->
	io:format("[~s] Player connecting...~n", [?MODULE]),
	spawn(fun() ->
		recv(Socket) end),
	{noreply, State};

handle_cast(stop, State) ->
	io:format("[~s] stopping~n", [?MODULE]),
	{stop, normal, State};

handle_cast(Message, State) ->
	io:format("[~s] received cast: ~p~n", [?MODULE, Message]),
	{noreply, State}.

handle_info(Message, State) ->
	case Message of
		_ ->
			io:format("[~s] received info: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


recv(Socket) ->
	{ok, Packet} = mc_erl_protocol:decode_packet(Socket),
	case Packet of
		{server_list_ping, [] } ->
			send(Socket, {disconnect,["A Minecraft Server§0§20"]});
			%gen_tcp:close(Socket);
		{handshake, [S]} ->
			{Name,_} = lists:split(string:str(S,";")-1,S),
			io:format("[~s] Player joining: ~s~n", [?MODULE, Name]),
			send(Socket, {handshake, ["-"]}),
			{ok, {login_request, [28, Name, "", 0, 0, 0, 0, 0]}}
				= mc_erl_protocol:decode_packet(Socket),
			% register client at entity manager to get entity id
			send(Socket, {login_request, [100, "", "DEFAULT", 1, 0, 0, 0, 100]}),
			send(Socket, {spawn_position, [0, 0, 0]}),
			LoadedChunks = check_chunks(Socket, {0, 0}),
			send(Socket, {player_position_look, [0,6,5,0,0,0,1]}),
			spawn_link(fun() -> keep_alive_sender(Socket) end),
			recv(Socket, LoadedChunks)
	end.

recv(Socket, LoadedChunks) ->
	case mc_erl_protocol:decode_packet(Socket) of
		{error,closed} ->
			% remove players' entity id etc.
			io:format("[~s] Lost connection~n", [?MODULE]);
		{ok, Packet} ->
			case Packet of
				{keep_alive, [_]} ->
					recv(Socket, LoadedChunks);
				{player, OnGround} ->
					recv(Socket, LoadedChunks);
				{player_position, [X, Y, Stance, Z, OnGround]} ->
					recv(Socket, check_chunks(Socket, {X, Y, Z}, LoadedChunks));
				{player_look, [Yaw, Pitch, OnGround]} ->
					recv(Socket, LoadedChunks);
				{player_position_look, [X, Y, Stance, Z, Yaw, Pitch, OnGround]} ->
					recv(Socket, check_chunks(Socket, {X, Y, Z}, LoadedChunks));
				{disconnect, [Message]} ->
					% remove players' entity id etc.
					io:format("[~s] A player disconnected: \"~s\"~n", [?MODULE, Message]),
					gen_tcp:close(Socket);
				_ ->
					io:format("[~s] unhandled packet: ~p~n", [?MODULE, Packet]),
					recv(Socket, LoadedChunks)
			end
	end.

% dummy sender, doesn't check for reply
keep_alive_sender(Socket) ->
	send(Socket, {keep_alive,  [0]}),
	receive after 1000 -> ok end,
	keep_alive_sender(Socket).


send(Socket, Packet) ->
	gen_tcp:send(Socket, mc_erl_protocol:encode_packet(Packet)).

check_chunks(Socket, PlayerChunk) ->
	check_chunks(Socket, PlayerChunk, sets:new()).

check_chunks(Socket, PlayerChunk, LoadedChunks) ->
	NeededChunks = mc_erl_chunk_manager:chunks_in_range(PlayerChunk, 3),
	unload_chunks(Socket, sets:to_list(sets:subtract(LoadedChunks, NeededChunks))),
	load_chunks(Socket, sets:to_list(sets:subtract(NeededChunks, LoadedChunks))),
	NeededChunks.

load_chunks(_Socket, []) -> ok;
load_chunks(Socket, [{X, Z}|Rest]) ->
	send(Socket, {pre_chunk, [X, Z, 1]}),
	ChunkData = mc_erl_chunk_manager:get_chunk({X, Z}),
	send(Socket, {map_chunk, [X, Z, {parsed, ChunkData}]}),
	load_chunks(Socket, Rest);
load_chunks(Socket, ChunksSet) ->
	Chunks = sets:to_list(ChunksSet),
	load_chunks(Socket, Chunks).

unload_chunks(_Socket, []) -> ok;
unload_chunks(Socket, [{X, Z}|Rest]) ->
	send(Socket, {pre_chunk, [X, Z, 0]}),
	unload_chunks(Socket, Rest).




