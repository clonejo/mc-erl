-module(mc_erl_config).
-behaviour(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% api functions
-export([start_link/0, get/2, set/2, reload/0]).

-define(CONFIG_FILE, "../server.conf"). % change pwd to have the server look elsewhere

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key, Default) ->
	gen_server:call(?MODULE, {get, Key, Default}).

%% not implemented yet
set(Key, Value) ->
	gen_server:call(?MODULE, {set, Key, Value}).

reload() ->
	gen_server:cast(?MODULE, reload).

% gen server stuff
init([]) ->
	io:format("[~s] starting~n", [?MODULE]),
	Entries = ets:new(void, [set, private]),
	try
		load_file(Entries, ?CONFIG_FILE)
	catch
		error:_ -> io:format("[~s] can't access server.conf~n", [?MODULE])
	end,
	{ok, Entries}.

handle_call({get, Key, Default}, _From, Entries) ->
	{reply, case ets:lookup(Entries, Key) of
		[] -> Default;
		[{Key, Value}] -> Value
	end, Entries};

handle_call(Message, _From, State) ->
	case Message of
		_ ->
			io:format("[~s] received call: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.

handle_cast(reload, Entries) ->
	load_file(Entries, ?CONFIG_FILE),
	{noreply, Entries};

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

% server internal stuff

%% overwrites all options used in the config file, other options are NOT deleted!
load_file(Entries, File) ->
	{ok, E} = file:consult(File),
	lists:map(fun(T) -> ets:insert(Entries, T) end, E).


