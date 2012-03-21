-module(mc_erl_entity_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0, register_player/1, delete_player/1, move_entity/2,
         get_all_players/0, get_player/1, broadcast/1, broadcast_local/2]).

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

%% Player = player name or record
delete_player(Player) ->
	gen_server:call(?MODULE, {delete_player, Player}).

move_entity(Eid, [_X, _Y, _Z, _Pitch, _Yaw]=NewLocation) ->
	gen_server:call(?MODULE, {move_entity, Eid, NewLocation}).

get_all_players() ->
	gen_server:call(?MODULE, get_all_players).

get_player(Name) ->
	gen_server:call(?MODULE, {get_player, Name}).

broadcast(Message) ->
	gen_server:cast(?MODULE, {broadcast, Message}).

broadcast_local(_Eid, Message) -> % a placeholder for a real local-only event
	gen_server:cast(?MODULE, {broadcast, Message}).

% gen_server callbacks
init([]) ->
	io:format("[~s] starting~n", [?MODULE]),
	%Entities = ets:new(entities, [set, private]),
	{ok, #state{players=ets:new(players, [set, private, {keypos, 3}])}}.

handle_call({register_player, Player}, _From, State) ->
	Eid = State#state.next_eid,
	NewPlayer = Player#player{eid=Eid},
	case ets:insert_new(State#state.players, NewPlayer) of
		false -> {reply, {error, name_in_use}, State};
		true -> {reply, NewPlayer, State#state{next_eid=Eid+1}}
	end;

handle_call({delete_player, Name}, _From, State) when is_list(Name) ->
	{reply, ets:delete(State#state.players, Name), State};

handle_call({delete_player, Player}, _From, State) ->
	{reply, ets:delete(State#state.players, Player#player.name), State};

handle_call(get_all_players, _From, State) ->
	Players = ets:tab2list(State#state.players),
	{reply, Players, State};

handle_call({get_player, Name}, _From, State) ->
	[Player] = ets:lookup(State#state.players, Name),
	{reply, Player, State};

handle_call(Message, _From, State) ->
	case Message of
		_ ->
			io:format("[~s] received call: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.

handle_cast(stop, State) ->
	io:format("[~s] stopping~n", [?MODULE]),
	{stop, normal, State};

handle_cast({broadcast, Message}, State) ->
	broadcast(Message, State),
	{noreply, State};

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

broadcast(Message, State) ->
	lists:map(fun(X) -> X#player.player_logic ! Message end, ets:tab2list(State#state.players)).
