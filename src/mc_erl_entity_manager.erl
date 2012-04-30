-module(mc_erl_entity_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0, register_player/1, delete_player/1, register_dropped_item/2,
         move_entity/2, delete_entity/1, entity_details/1, get_all_entities/0, get_all_players/0,
         get_player/1, player_count/0, broadcast/1, broadcast_local/2, broadcast_visible/2]).
-define(dev, void).
-include("records.hrl").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {next_eid=0}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, stop).

register_player(Player) -> gen_server:call(?MODULE, {register_player, Player, self()}).
delete_player(Player) when is_record(Player, player) orelse is_list(Player) -> gen_server:call(?MODULE, {delete_player, Player}).

register_dropped_item(Entity, Velocity) when is_record(Entity, entity)-> gen_server:call(?MODULE, {register_dropped_item, Entity, Velocity, self()}).

move_entity(Eid, {_X, _Y, _Z, _Pitch, _Yaw}=NewLocation) ->
	case entity_details(Eid) of
		undefined -> delete_entity(Eid);
		Entity ->
			NewEntity = Entity#entity{location=NewLocation},
			{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(NewEntity) end),
			broadcast({update_entity_position, {NewEntity}})
	end.

delete_entity(Entity) when is_record(Entity, entity) -> delete_entity(Entity#entity.eid);
delete_entity(Eid) when is_integer(Eid) ->
	broadcast({delete_entity, Eid}),
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:delete({entity, Eid}) end),
	ok.

get_all_players() ->
	{atomic, Players} = mnesia:transaction(fun() -> mnesia:match_object(#entity{type=player, _='_'}) end),
	Players.
	
player_count() ->
	length(get_all_players()).
	
get_all_entities() ->
	{atomic, Entities} = mnesia:transaction(fun() -> mnesia:match_object(#entity{_='_'}) end),
	Entities.

get_player(Name) when is_list(Name), length(Name) > 0 ->
	{atomic, PlayerL} = mnesia:transaction(fun() -> mnesia:index_read(entity, Name, #entity.name) end),
	PlayerL.

entity_details(Eid) ->
	case mnesia:transaction(fun() -> mnesia:read(entity, Eid) end) of
		{atomic, [Entity]} -> Entity;
		{atomic, []} -> undefined
	end.

broadcast(Message) ->
	lists:map(fun(X) -> mc_erl_player_logic:packet(X#entity.logic, Message) end, get_all_entities()).

broadcast_local(_Eid, Message) -> % a placeholder for a real local-only event
	broadcast(Message).

broadcast_visible({_X, _Y, _Z}, Message) -> % a placeholder for a ranged event
	broadcast(Message).

% gen_server callbacks
init([]) ->
	io:format("[~s] starting~n", [?MODULE]),
	%Entities = ets:new(entities, [set, private]),
	case mnesia:create_table(entity, [{attributes, record_info(fields, entity)},
	                                  {type, set}, {index, [name]}]) of
		{atomic, ok} -> ok;
		{aborted,{already_exists,entity}} -> ok
	end,
	{ok, #state{}}.

handle_call({register_player, Player, Logic}, _From, State) when is_record(Player, player), is_pid(Logic) ->
	Eid = State#state.next_eid,
	NewPlayer = escape_player_name(Player#player{eid=Eid}, Eid),
	case get_player(NewPlayer#player.name) of
		[_] -> {reply, {error, name_in_use}, State};
		[] ->
			Entity = #entity{eid=Eid, name=NewPlayer#player.name, type=player, logic=Logic, location = NewPlayer#player.location},
			{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Entity) end),
			broadcast({new_entity, Entity}),
			{reply, NewPlayer, State#state{next_eid=Eid+1}}
	end;

handle_call({delete_player, Name}, _From, State) when is_list(Name) ->
	case get_player(Name) of
		[Player] -> 
			broadcast({delete_entity, Player#entity.eid}),
            
			{atomic, ok} = mnesia:transaction(fun() -> mnesia:delete({entity, Player#entity.eid}) end),
			{reply, ok, State};
		[] ->
			{reply, name_not_found, State}
	end;

handle_call({delete_player, Player}, _From, State) when is_record(Player, player) ->
	case entity_details(Player#player.eid) of
		undefined -> ok;
		Entity -> broadcast({delete_entity, Entity})
	end,
	case mnesia:transaction(fun() -> mnesia:delete({entity, Player#player.eid}) end) of
		{atomic, ok} ->	{reply, ok, State};
		{aborted, _} -> {reply, not_found, State}
	end;

% register_dropped_item
handle_call({register_dropped_item, Entity, InitialVelocity, Logic}, _From, State) ->
	Eid = State#state.next_eid,
	NewEntity = Entity#entity{eid=Eid, logic=Logic},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(NewEntity) end),
	broadcast({new_entity, NewEntity}),
    broadcast({set_entity_speed, NewEntity, InitialVelocity}),
	{reply, NewEntity, State#state{next_eid=Eid+1}};

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


-ifdef(dev).
escape_player_name(Player, Eid) ->
	Player#player{name=Player#player.name ++ "#" ++ integer_to_list(Eid)}.
-else.
escape_player_name(Player, _Eid) ->
	Player.
-endif.

