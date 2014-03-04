%% @copyright 2012-2013 Gregory Fefelov, Feiko Nanninga

-module(mc_erl_proxy).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

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


handle_cast({new_connection, ClientSocket}, State) ->
	io:format("[~s] Player connecting...~n", [?MODULE]),
	{ok, ServerSocket} =
		gen_tcp:connect({127,0,0,1}, 25565,
				[binary, {reuseaddr, true}, {active, false},
				 {packet, raw}]),
	spawn(fun() ->
		random:seed(erlang:now()),
		FileName = "../log/" ++ integer_to_list(random:uniform(999999)) ++ ".log",
		io:format("[~s] logging to: ~p~n", [?MODULE, FileName]),
		{ok, File} = file:open(FileName, write),
		LogHandler = spawn(fun() -> log_handler(File) end),
		spawn_link(fun() -> server_to_client(ClientSocket, ServerSocket, LogHandler) end),
		client_to_server(ClientSocket, ServerSocket, LogHandler) end),
	{noreply, State};

handle_cast(stop, State) ->
	io:format("[~s] stopping~n", [?MODULE]),
	{stop, normal, State};

handle_cast(Message, State) ->
	io:format("[~s] received cast: ~p~n", [?MODULE, Message]),
	{noreply, State}.

log_handler(File) ->
	receive
		{log, String, Args} ->
			io:format(File, String, Args),
			log_handler(File)
		%after 5000 ->
		%	file:close(File)
	end.

server_to_client(ClientSocket, ServerSocket, LogHandler) ->
	case mc_erl_protocol:decode_packet(ServerSocket) of
		{ok, Packet} ->
			LogHandler ! {log, "[C<-S] packet: ~p~n", [Packet]},
			Bin = mc_erl_protocol:encode_packet(Packet),
			gen_tcp:send(ClientSocket, Bin),
			server_to_client(ClientSocket, ServerSocket, LogHandler);
		{error, Reason} ->
			io:format("[~s] [C<-S] stopping with ~p~n", [?MODULE, {error, Reason}]),
			{error, Reason}
	end.

client_to_server(ClientSocket, ServerSocket, LogHandler) ->
	case mc_erl_protocol:decode_packet(ClientSocket) of
		{ok, Packet} ->
			LogHandler ! {log, "[C->S] packet: ~p~n", [Packet]},
			Bin = mc_erl_protocol:encode_packet(Packet),
			gen_tcp:send(ServerSocket, Bin),
			client_to_server(ClientSocket, ServerSocket, LogHandler);
		{error, Reason} ->
			io:format("[~s] [C->S] stopping with ~p~n", [?MODULE, {error, Reason}]),
			{error, Reason}
	end.

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


