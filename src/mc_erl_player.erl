-module(mc_erl_player).
-export([init_player/1]).

init_player(Socket) ->
	proc_lib:init_ack(self()),
	recv(Socket).

recv(Socket) ->
	{ok, Packet} = mc_erl_protocol:decode_packet(Socket),
	case Packet of
		{server_list_ping, [] } ->
			send(Socket, {disconnect,[lists:flatten(["a ...err... lang server!",167, "0", 167, "100"])]});
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
				{player_digging, [0, X, Y, Z, _]} ->
					mc_erl_chunk_manager:set_block({X, Y, Z}, {0, 0}),
					recv(Socket, LoadedChunks);
				{player_block_placement, [X, Y, Z, Direction, {BlockId, 1, Metadata}]} ->
					mc_erl_chunk_manager:set_block({X, Y, Z, Direction}, {BlockId, Metadata}),
					recv(Socket, LoadedChunks);
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

