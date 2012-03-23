-module(mc_erl_player_logic).
% only pure erlang, only pure hardcore
-export([start_logic/2, packet/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {writer, player, mode=creative, chunks=none, 
                known_entities=dict:new(), last_tick, pos={0.5, 70, 0.5, 0, 0}}).%pos = {X, Y, Z, Yaw, Pitch}

-include("records.hrl").

start_logic(Writer, Name) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [Writer, Name], []),
	Pid.

packet(Logic, Packet) ->
	gen_server:cast(Logic, Packet).

init([Writer, Name]) ->
	{ok, #state{writer=Writer, player=#player{name=Name, player_logic=self()}}}.

terminate(_Reason, State) ->
	State#state.writer ! stop,
	mc_erl_chat:broadcast(State#state.player#player.name ++ " has left the server."),
	mc_erl_entity_manager:delete_player(State#state.player),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Info, State) ->
	io:format("[~s] got unknown info: ~p~n", [?MODULE, Info]),
	{noreply, State}.

handle_call(Req, _From, State) ->
	io:format("[~s] got unknown packet: ~p~n", [?MODULE, Req]),
	{noreply, State}.
	
handle_cast(Req, State) ->
	Writer = State#state.writer,
	RetState = case Req of
		% protocol reactions begin
		login_sequence ->
			IsValid = mc_erl_chat:is_valid_nickname(State#state.player#player.name),
			case IsValid of
				true ->	
					case mc_erl_entity_manager:register_player(State#state.player) of
						{error, name_in_use} ->
							io:format("[~s] Someone with the same name is already logged in, kicked~n", [?MODULE]),
							write(Writer, {disconnect, ["Someone with the same name is already logged in :("]}),
							{disconnect, "same name logged in", State}; % different atom here?
		
						NewPlayer ->
							write(Writer, {login_request, [NewPlayer#player.eid, "", "DEFAULT", 1, 0, 0, 0, 100]}),
							write(Writer, {spawn_position, [0, 0, 0]}),
							{X, Y, Z, Yaw, Pitch} = State#state.pos,
							
							Chunks = check_chunks(Writer, {X, Y, Z}),
							
							write(Writer, {player_position_look, [X,Y+1.62,Y,Z,Yaw,Pitch,1]}),
							mc_erl_chat:broadcast(NewPlayer#player.name ++ " has joined."),
							State#state{chunks=Chunks, player=NewPlayer}
					end;
				false ->
					io:format("[~s] Someone with the wrong nickname has tried to log in, kicked~n", [?MODULE]),
					write(Writer, {disconnect, ["Invalid username :("]}),
					{disconnect, "invalid username", State}
			end;
			
		{packet, {keep_alive, [_]}} ->
			State;
			
		{packet, {player, _OnGround}} ->
			State;
			
		{packet, {player_position, [X, Y, _Stance, Z, _OnGround]}} ->
			{_OldX, _OldY, _OldZ, Yaw, Pitch} = State#state.pos,
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, State),
			
			NewState = State#state{chunks=check_chunks(State#state.writer, {X, Y, Z}, State#state.chunks), pos=NewPos},
			NewState;
			
		{packet, {player_look, [Yaw, Pitch, _OnGround]}} ->
			{X, Y, Z, _OldYaw, _OldPitch} = State#state.pos,
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, State),
			State;
			
		{packet, {player_position_look, [X, Y, _Stance, Z, Yaw, Pitch, _OnGround]}} ->
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, State),
			
			%io:format("pos upd: ~p~n", [Position]),
			NewState = State#state{chunks=check_chunks(State#state.writer, {X, Y, Z}, State#state.chunks), pos=NewPos},
			NewState;
			
		{packet, {disconnect, [Message]}} ->
			{disconnect, Message, State};
		
		{packet, {player_digging, [0, X, Y, Z, _]}} ->
			mc_erl_chunk_manager:set_block({X, Y, Z}, {0, 0}),
			State;
		
		{packet, {player_block_placement, [-1, -1, -1, -1, {_BlockId, 1, _Metadata}]}} ->
			% handle held item state update (eating food etc.)
			State;
			
		{packet, {player_block_placement, [X, Y, Z, Direction, {BlockId, 1, Metadata}]}} when BlockId < 256 ->
			mc_erl_chunk_manager:set_block({X, Y, Z, Direction}, {BlockId, Metadata}),
			State;
		
		{packet, {chat_message, [Message]}} ->
			mc_erl_chat:broadcast(State#state.player, Message),
			State;
		
		{packet, UnknownPacket} ->
			io:format("[~s] unhandled packet: ~p~n", [?MODULE, UnknownPacket]),
			State;
		% protocol reactions end
		
		% chat
		{chat, Message} ->
			write(Writer, {chat_message, [Message]}),
			State;
		
		{tick, Tick} ->
			if
				(Tick rem 20) == 0 -> precise_positions(State);
				true -> ok
			end,
			FinalState = State#state{last_tick=Tick},
			FinalState;
		
		{block_delta, {X, Y, Z, BlockId, Metadata}} ->
			case in_range({X, Y, Z}, State) of
				true ->
					write(Writer, {block_change, [X, Y, Z, BlockId, Metadata]});
				false -> ok
			end,
			State;
		
		{delete_entity, Eid} ->
			NewState = delete_entity(Eid, State),
			NewState;
				
		
		{update_entity_position, {Eid, {_X, _Y, _Z, _Yaw, _Pitch}=NewLocation}} ->
			NewState = update_entity(Eid, NewLocation, State),
			NewState;
					
		
		UnknownMessage ->
			io:format("[~s] unknown message: ~p~n", [?MODULE, UnknownMessage]),
			State
	end,
	case RetState of
		{disconnect, Reason, DisconnectState} -> {stop, Reason, DisconnectState};
		Res -> {noreply, Res}
	end.

% ==== update entity location
precise_positions(State) ->
	Writer = State#state.writer,
	MyEid = State#state.player#player.eid,
	Entities = mc_erl_entity_manager:get_all_entities(),
	lists:foreach(fun(Entity) ->
			Eid = Entity#entity_data.eid,
			{X, Y, Z, Yaw, Pitch} = Entity#entity_data.location,
			FracYaw = trunc(Yaw*256/360),
			FracPitch = trunc(Pitch*256/360),
			write(Writer, {entity_teleport, [Eid, mc_erl_protocol:to_absint(X), mc_erl_protocol:to_absint(Y), mc_erl_protocol:to_absint(Z), FracYaw, FracPitch]})			
		end, lists:filter(fun(P) -> 
			{X, Y, Z, _Yaw, _Pitch} = P#entity_data.location,
			in_range({X, Y, Z}, State) and (MyEid /= P#entity_data.eid)
		end, Entities)).

update_entity(Eid, {X, Y, Z, Yaw, Pitch}, State) ->
	Writer = State#state.writer,
	if
		Eid == State#state.player#player.eid ->
			State;
		true -> case in_range({X, Y, Z}, State) of
			false ->
				case dict:is_key(Eid, State#state.known_entities) of 
					true -> delete_entity(Eid, State);
					false -> State
				end;
			true ->
				case dict:is_key(Eid, State#state.known_entities) of
					true ->
						{OldX, OldY, OldZ, _OldYaw, _OldPitch, Data} = dict:fetch(Eid, State#state.known_entities),
						DX = X - OldX,
						DY = Y - OldY,
						DZ = Z - OldZ,
						DDistance = lists:max([DX, DY, DZ]),
						FracYaw = trunc(Yaw*256/360),
						FracPitch = trunc(Pitch*256/360),
						
						ChangePackets = if
							DDistance >= 4 ->
								[{entity_teleport, [Eid, mc_erl_protocol:to_absint(X), mc_erl_protocol:to_absint(Y), mc_erl_protocol:to_absint(Z), FracYaw, FracPitch]}];
							true ->
								[{entity_look_move, [Eid, mc_erl_protocol:to_absint(DX), mc_erl_protocol:to_absint(DY), mc_erl_protocol:to_absint(DZ), FracYaw, FracPitch]},
								 {entity_head_look, [Eid, FracYaw]}]
						end,
						NewKnownEntities = dict:store(Eid, {X, Y, Z, Yaw, Pitch, Data}, State#state.known_entities),
						lists:map(fun(Packet) -> write(Writer, Packet) end, ChangePackets),
						State#state{known_entities=NewKnownEntities};
					false ->
						EntityData = mc_erl_entity_manager:entity_details(Eid),
						if
							EntityData#entity_data.type == player ->
								PName = EntityData#entity_data.metadata#player_metadata.name,
								PHolding = EntityData#entity_data.metadata#player_metadata.holding_item,
								write(Writer, {named_entity_spawn, [Eid, PName, mc_erl_protocol:to_absint(X), mc_erl_protocol:to_absint(Y), mc_erl_protocol:to_absint(Z), trunc(Yaw*256/360), trunc(Yaw*256/360), PHolding]}),
								NewKnownEntities = dict:store(Eid, {X, Y, Z, Yaw, Pitch, {}}, State#state.known_entities),
								State#state{known_entities=NewKnownEntities};
							true ->
								State
						end
				end
		end
	end.

write(Writer, Packet) -> Writer ! {packet, Packet}.

delete_entity(Eid, State) ->
	case dict:is_key(Eid, State#state.known_entities) of
		true ->	
			write(State#state.writer, {destroy_entity, [Eid]}),
			NewKnownEntities = dict:erase(Eid, State#state.known_entities),
			State#state{known_entities=NewKnownEntities};
		false ->
			State
	end.

% ==== Send position to everyone
broadcast_position(Position, State) ->
	Eid = State#state.player#player.eid,
	mc_erl_entity_manager:broadcast_local(Eid, {update_entity_position, {Eid, Position}}).

% ==== Checks if location is in visible range
in_range({X, Y, Z}, State) ->
	ChunkPos = mc_erl_chunk_manager:coord_to_chunk({X, Y, Z}),
	sets:is_element(ChunkPos, State#state.chunks).

% ==== Chunks loading
check_chunks(Writer, PlayerChunk) ->
	check_chunks(Writer, PlayerChunk, sets:new()).

check_chunks(Writer, PlayerChunk, LoadedChunks) ->
	NeededChunks = mc_erl_chunk_manager:chunks_in_range(PlayerChunk, 7),
	unload_chunks(Writer, sets:to_list(sets:subtract(LoadedChunks, NeededChunks))),
	load_chunks(Writer, sets:to_list(sets:subtract(NeededChunks, LoadedChunks))),
	NeededChunks.

load_chunks(_Writer, []) -> ok;
load_chunks(Writer, [{X, Z}|Rest]) ->
	write(Writer, {pre_chunk, [X, Z, true]}),
	ChunkData = mc_erl_chunk_manager:get_chunk({X, Z}),
	write(Writer, {map_chunk, [X, Z, {parsed, ChunkData}]}),
	load_chunks(Writer, Rest);
load_chunks(Writer, ChunksSet) ->
	Chunks = sets:to_list(ChunksSet),
	load_chunks(Writer, Chunks).

unload_chunks(_Writer, []) -> ok;
unload_chunks(Writer, [{X, Z}|Rest]) ->
	write(Writer, {pre_chunk, [X, Z, false]}),
	unload_chunks(Writer, Rest).

