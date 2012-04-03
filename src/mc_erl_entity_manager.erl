-module(mc_erl_entity_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0, register_player/1, delete_player/1, move_entity/2, entity_details/1,
         get_all_entities/0, get_all_players/0, get_player/1, player_count/0, broadcast/1, broadcast_local/2, broadcast_visible/2]).
%-define(dev, void).
-include("records.hrl").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {players, entities, next_eid=0}).

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

get_all_entities() ->
	gen_server:call(?MODULE, get_all_entities).

get_player(Name) ->
	gen_server:call(?MODULE, {get_player, Name}).

player_count() ->
	gen_server:call(?MODULE, player_count).

entity_details(Eid) ->
	gen_server:call(?MODULE, {entity_details, Eid}).

broadcast(Message) ->
	gen_server:cast(?MODULE, {broadcast, Message}).

broadcast_local(_Eid, Message) -> % a placeholder for a real local-only event
	gen_server:cast(?MODULE, {broadcast, Message}).

broadcast_visible({_X, _Y, _Z}, Message) -> % a placeholder for a ranged event
	gen_server:cast(?MODULE, {broadcast, Message}).

% gen_server callbacks
init([]) ->
	io:format("[~s] starting~n", [?MODULE]),
	%Entities = ets:new(entities, [set, private]),
	{ok, #state{players=ets:new(players, [set, private, {keypos, 3}]),
		entities=ets:new(entities, [set, private, {keypos, 2}])}}.

handle_call({register_player, Player}, _From, State) ->
	Eid = State#state.next_eid,
	NewPlayer = escape_player_name(Player#player{eid=Eid}, Eid),
	case ets:insert_new(State#state.players, NewPlayer) of
		false -> {reply, {error, name_in_use}, State};
		true ->
			ets:insert_new(State#state.entities, #entity_data{eid=Eid, type=player, metadata=#player_metadata{name=NewPlayer#player.name, holding_item=0}}),
			broadcast({new_player, Player}),
			{reply, NewPlayer, State#state{next_eid=Eid+1}}
	end;

handle_call({delete_player, Name}, _From, State) when is_list(Name) ->
	case ets:lookup(State#state.players, Name) of
		[Player] -> 
			broadcast({delete_entity, Player#player.eid}),
			ets:delete(State#state.entities, Player#player.eid),
			{reply, ets:delete(State#state.players, Name), State};
		[] ->
			{reply, name_not_found, State}
	end;

handle_call({delete_player, Player}, _From, State) ->
	broadcast({delete_player, Player}),
	ets:delete(State#state.entities, Player#player.eid),
	{reply, ets:delete(State#state.players, Player#player.name), State};

handle_call(player_count, _From, State) ->
	{reply, ets:info(State#state.players, size), State};

handle_call(get_all_players, _From, State) ->
	Players = ets:tab2list(State#state.players),
	{reply, Players, State};

handle_call(get_all_entities, _From, State) ->
	Entities = ets:tab2list(State#state.entities),
	{reply, Entities, State};

handle_call({get_player, Name}, _From, State) ->
	[Player] = ets:lookup(State#state.players, Name),
	{reply, Player, State};

handle_call({entity_details, Eid}, _From, State) ->
	[Details] = ets:lookup(State#state.entities, Eid),
	{reply, Details, State};

handle_call(Message, _From, State) ->
	case Message of
		_ ->
			io:format("[~s] received call: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.

handle_cast(stop, State) ->
	io:format("[~s] stopping~n", [?MODULE]),
	{stop, normal, State};

handle_cast({broadcast, {update_entity_position, {Eid, Position}}=Message}, State) ->
	[Player] = ets:lookup(State#state.entities, Eid),
	NewPlayer = Player#entity_data{location=Position},
	ets:insert(State#state.entities, NewPlayer),
	broadcast(Message, State),
	{noreply, State};
	
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
	lists:map(fun(X) -> mc_erl_player_logic:packet(X#player.player_logic, Message) end, ets:tab2list(State#state.players)).


%% functions used by gen_server functions:
-ifdef(dev).
escape_player_name(Player, Eid) ->
	Player#player{name=Player#player.name ++ "#" ++ integer_to_list(Eid)}.
-else.
escape_player_name(Player, _Eid) ->
	Player.
-endif.

