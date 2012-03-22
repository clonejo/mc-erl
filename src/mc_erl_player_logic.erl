-module(mc_erl_player_logic).
% only pure erlang, only pure hardcore
-export([init_logic/2]).
-record(state, {writer, player, mode=creative, chunks=none, 
                known_entities=dict:new(), last_tick, pos={0.5, 70, 0.5, 0, 0}}).%pos = {X, Y, Z, Yaw, Pitch}

-include("records.hrl").

init_logic(Writer, Name) ->
	loop(#state{writer=Writer, player=#player{name=Name, player_logic=self()}}).

write(Writer, Packet) -> Writer ! {packet, Packet}.

loop(State) ->
	Writer = State#state.writer,
	receive
		% protocol reactions begin
		login_sequence ->
			case mc_erl_entity_manager:register_player(State#state.player) of
				{error, name_in_use} ->
					io:format("[~s] Someone with the same name is already logged in, kicked~n", [?MODULE]),
					write(Writer, {disconnect, ["Someone with the same name is already logged in :("]}),
					exit(disconnect); % different atom here?

				NewPlayer ->
					write(Writer, {login_request, [NewPlayer#player.eid, "", "DEFAULT", 1, 0, 0, 0, 100]}),
					write(Writer, {spawn_position, [0, 0, 0]}),
					{X, Y, Z, Yaw, Pitch} = State#state.pos,
					
					Chunks = check_chunks(Writer, {X, Y, Z}),
					
					write(Writer, {player_position_look, [X,Y+1.62,Y,Z,Yaw,Pitch,1]}),
					mc_erl_chat:broadcast(NewPlayer#player.name ++ " has joined."),
					loop(State#state{chunks=Chunks, player=NewPlayer})
			end;
			
		{packet, {keep_alive, [_]}} ->
			loop(State);
			
		{packet, {player, _OnGround}} ->
			loop(State);
			
		{packet, {player_position, [X, Y, _Stance, Z, _OnGround]}} ->
			{_OldX, _OldY, _OldZ, Yaw, Pitch} = State#state.pos,
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, State),
			
			NewState = State#state{chunks=check_chunks(State#state.writer, {X, Y, Z}, State#state.chunks), pos=NewPos},
			loop(NewState);
			
		{packet, {player_look, [Yaw, Pitch, _OnGround]}} ->
			{X, Y, Z, _OldYaw, _OldPitch} = State#state.pos,
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, State),
			loop(State);
			
		{packet, {player_position_look, [X, Y, _Stance, Z, Yaw, Pitch, _OnGround]=Position}} ->
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, State),
			
			%io:format("pos upd: ~p~n", [Position]),
			NewState = State#state{chunks=check_chunks(State#state.writer, {X, Y, Z}, State#state.chunks), pos=NewPos},
			loop(NewState);
			
		{packet, {disconnect, [Message]}} ->
			io:format("[~s] A player disconnected: \"~s\"~n", [?MODULE, Message]),
			mc_erl_entity_manager:delete_player(State#state.player),
			mc_erl_chat:broadcast(State#state.player#player.name ++ " has left the server."),
			exit(disconnect);
		
		{packet, {player_digging, [0, X, Y, Z, _]}} ->
			mc_erl_chunk_manager:set_block({X, Y, Z}, {0, 0}),
			mc_erl_entity_manager:broadcast_local(State#state.player#player.eid, {block_delta, {X, Y, Z, 0, 0}}),
			loop(State);
		
		{packet, {player_block_placement, [-1, -1, -1, -1, {_BlockId, 1, _Metadata}]}} ->
			% handle held item state update (eating food etc.)
			loop(State);
			
		{packet, {player_block_placement, [X, Y, Z, Direction, {BlockId, 1, Metadata}]}} when BlockId < 256 ->
			mc_erl_chunk_manager:set_block({X, Y, Z, Direction}, {BlockId, Metadata}),
			{AX, AY, AZ} = mc_erl_chunk_manager:undirectional_block_coord({X, Y, Z, Direction}),
			mc_erl_entity_manager:broadcast_local(State#state.player#player.eid, {block_delta, {AX, AY, AZ, BlockId, Metadata}}),
			loop(State);
		
		{packet, {chat_message, [Message]}} ->
			mc_erl_chat:broadcast(State#state.player, Message),
			loop(State);
		
		{packet, UnknownPacket} ->
			io:format("[~s] unhandled packet: ~p~n", [?MODULE, UnknownPacket]),
			loop(State);
		% protocol reactions end
		
		% chat
		{chat, Message} ->
			write(Writer, {chat_message, [Message]}),
			loop(State);
		
		{tick, Tick} ->
			NewState = State#state{last_tick=Tick},
			loop(NewState);
		
		{block_delta, {X, Y, Z, BlockId, Metadata}} ->
			case in_range({X, Y, Z}, State) of
				true ->
					write(Writer, {block_change, [X, Y, Z, BlockId, Metadata]});
				false -> ok
			end,
			loop(State);
		
		{delete_entity, Eid} ->
			NewState = case dict:is_key(Eid, State#state.known_entities) of
				true ->	
					write(Writer, {destroy_entity, [Eid]}),
					NewKnownEntities = dict:erase(Eid, State#state.known_entities),
					State#state{known_entities=NewKnownEntities};
				false ->
					State
			end,
			loop(NewState);
				
		
		{update_entity_position, {Eid, {X, Y, Z, Yaw, Pitch}}} ->
			NewState = if
				Eid == State#state.player#player.eid ->
					State;
				true -> case in_range({X, Y, Z}, State) of
					false -> State;
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
			end,
			loop(NewState);
					
		
		UnknownMessage ->
			io:format("[~s] unknown message: ~p~n", [?MODULE, UnknownMessage]),
			loop(State)
		
		% {update_entity, Eid, {X, Y, Z, Yaw, Pitch}=NewPosition} ->
			% case find(Eid, State#state.entities) of
				% {ok, {player, PName}} -> 
	end.
		
broadcast_position(Position, State) ->
	Eid = State#state.player#player.eid,
	mc_erl_entity_manager:broadcast_local(Eid, {update_entity_position, {Eid, Position}}).

in_range({X, Y, Z}, State) ->
	ChunkPos = mc_erl_chunk_manager:coord_to_chunk({X, Y, Z}),
	sets:is_element(ChunkPos, State#state.chunks).

check_chunks(Writer, PlayerChunk) ->
	check_chunks(Writer, PlayerChunk, sets:new()).

check_chunks(Writer, PlayerChunk, LoadedChunks) ->
	NeededChunks = mc_erl_chunk_manager:chunks_in_range(PlayerChunk, 7),
	unload_chunks(Writer, sets:to_list(sets:subtract(LoadedChunks, NeededChunks))),
	load_chunks(Writer, sets:to_list(sets:subtract(NeededChunks, LoadedChunks))),
	NeededChunks.

load_chunks(_Writer, []) -> ok;
load_chunks(Writer, [{X, Z}|Rest]) ->
	write(Writer, {pre_chunk, [X, Z, 1]}),
	ChunkData = mc_erl_chunk_manager:get_chunk({X, Z}),
	write(Writer, {map_chunk, [X, Z, {parsed, ChunkData}]}),
	load_chunks(Writer, Rest);
load_chunks(Writer, ChunksSet) ->
	Chunks = sets:to_list(ChunksSet),
	load_chunks(Writer, Chunks).

unload_chunks(_Writer, []) -> ok;
unload_chunks(Writer, [{X, Z}|Rest]) ->
	write(Writer, {pre_chunk, [X, Z, 0]}),
	unload_chunks(Writer, Rest).

