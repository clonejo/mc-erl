-module(mc_erl_entity_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0, register_player/1, get_player/1]).

-include("records.hrl").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {players, next_eid=0}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

register_player(Player) ->
	gen_server:call(?MODULE, {register_player, Player}).

get_player(Name) ->
	gen_server:call(?MODULE, {get_player, Name}).

% gen_server callbacks
init([]) ->
	io:format("[~s] starting~n", [?MODULE]),
	%Entities = ets:new(entities, [set, private]),
	{ok, #state{players=ets:new(foo, [set, private])}}.

handle_call({register_player, Player}, _From, State) ->
	Eid = State#state.next_eid,
	NewPlayer = Player#player{eid=Eid},
	case ets:insert_new(State#state.players, {Player#player.name, NewPlayer}) of
		false -> {reply, {error, name_in_use}};
		true -> {reply, NewPlayer, State#state{next_eid=Eid+1}}
	end;

handle_call({get_player, Name}, _From, State) ->
	[Player] = ets:lookup(State#state.players, Name),
	Player;

handle_call(Message, _From, State) ->
	case Message of
		_ ->
			io:format("[~s] received call: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.

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


