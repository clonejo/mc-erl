-module(mc_erl_server).
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

handle_cast({new_connection, Socket}, State) ->
	io:format("[~s] Player connecting...~n", [?MODULE]),
	Pid = proc_lib:start(mc_erl_player_core, init_player, [Socket]),
	gen_tcp:controlling_process(Socket, Pid),
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






