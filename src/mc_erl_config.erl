-module(mc_erl_config).
-behaviour(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% api functions
-export([start_link/0, get/1, set/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
	gen_server:call(?MODULE, {get, Key}).

%% not implemented yet
set(Key, Value) ->
	gen_server:call(?MODULE, {set, Key, Value}).

% gen server stuff
init([]) ->
	io:format("[~s] starting~n", [?MODULE]),
	{ok, E} = file:consult("../server.conf"),
	Entries = ets:new(void, [set, private]),
	lists:map(fun(T) -> ets:insert(Entries, T) end, E),
	{ok, Entries}.

handle_call({get, Key}, _From, Entries) ->
	{reply, case ets:lookup(Entries, Key) of
		[] -> undefined;
		[{Key, Value}] -> Value
	end, Entries};

handle_call(Message, _From, State) ->
	case Message of
		_ ->
			io:format("[~s] received call: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.

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
