-module(mc_erl_dropped_item).

-export([new/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {entity, falling={0,0,0}, last_tick}).

-include("records.hrl").

-define(pick_up_range, 1).       % unit: m
-define(gravity_acc, 0,0245).    % unit: m/tick^2

new(Entity) when is_record(Entity, entity), Entity#entity.type =:= dropped_item ->
	{ok, Pid} = gen_server:start_link(?MODULE, Entity, []),
	Pid.

init(Entity) ->
	process_flag(trap_exit, true),
	{ok, #state{entity=mc_erl_entity_manager:register_dropped_item(Entity)}}.

terminate(_Reason, State) ->
	mc_erl_entity_manager:delete_entity(State#state.entity),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Info, State) ->
	io:format("[~s] got unknown info: ~p~n", [?MODULE, Info]),
	{noreply, State}.

handle_call(Req, _From, State) ->
	io:format("[~s] got unknown call: ~p~n", [?MODULE, Req]),
	{noreply, State}.

handle_cast(Req, State) ->
	MyEntity = State#state.entity,
	MyEid = MyEntity#entity.eid,
	RetState = case Req of
		% chat
		{chat, _Message} -> State;
		
		{animate, _Eid, _AnimationId} -> State;
		
		{entity_metadata, _Eid, _Metadata} -> State;
			
		{tick, Tick} ->
			% movement simulation, don't broadcast position updates
			case State#state.falling of
				_ -> State#state{last_tick=Tick}
				%{VX, VY, VZ} -> % velocity in m per tick
				%	{X, Y, Z, _, _} = MyEntity#entity.location,
				%	NVX = VX + ?gravity_acc,
				%	NVY = VY + ?gravity_acc,
				%	NVZ = VZ + ?gravity_acc,
				%	NewLocation = {X, Y, Z, 0, 0},
				%	State#state{entity=MyEntity#entity{location=NewLocation}, last_tick=Tick}
			end;
		
		% don't use these for movement simulation, register for events at chunk_manager
		{block_delta, _} -> State;
		{update_column, _} -> State;
		
		% adds or removes a player on the player list
		{player_list, _Player, _Mode} -> State;
		
		% === entity messages ===
		{new_entity, Entity} ->
			pick_up_check(Entity, State);
		
		{delete_entity, Eid} ->
			case MyEid =:= Eid of
				true -> {stop, normal, entity_deleted};
				false -> State
			end;
		
		{update_entity_position, {Entity}} when is_record(Entity, entity) ->
			pick_up_check(Entity, State);
		
		UnknownMessage ->
			io:format("[~s] unknown message: ~p~n", [?MODULE, UnknownMessage]),
			State
	end,
	case RetState of
		% ...
		
		% right path
		Res -> {noreply, Res}
	end.

pick_up_check(Entity, State) when Entity#entity.type =:= player ->
	case in_range(Entity#entity.location, State) of
		true -> io:format("~p: player in range!~n", [State#state.entity#entity.eid]);
		false -> ok
	end,
	State;
pick_up_check(_, State) -> State.
	
% ==== Checks if (a player's) location is in pick up range
in_range({X, Y, Z, _, _}, State) ->
	{MyX, MyY, MyZ, _, _} = State#state.entity#entity.location,
	math:sqrt(math:pow(MyX-X,2) + math:pow(MyY-Y,2) + math:pow(MyZ-Z,2)) < ?pick_up_range.


