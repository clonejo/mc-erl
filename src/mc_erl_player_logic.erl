-module(mc_erl_player_logic).
% only pure erlang, only pure hardcore
-export([start_logic/2, packet/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {writer, player, mode=creative, chunks=none, cursor_item=empty, logged_in=false,
                known_entities=dict:new(), last_tick, pos={0.5, 70, 0.5, 0, 0}}).%pos = {X, Y, Z, Yaw, Pitch}

-record(ke_metadata, {relocations=0}).

-include("records.hrl").

start_logic(Writer, Name) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [Writer, Name], []),
	Pid.

packet(Logic, Packet) ->
	gen_server:cast(Logic, Packet).

init([Writer, Name]) ->
	process_flag(trap_exit, true),
	{ok, #state{writer=Writer, player=#player{name=Name, player_logic=self()}}}.

terminate(_Reason, State) ->
	State#state.writer ! stop,
	case State#state.logged_in of
		true ->
			mc_erl_chat:broadcast(State#state.player#player.name ++ " has left the server."),
			mc_erl_entity_manager:delete_player(State#state.player);
		false -> ok
	end,
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
	MyPlayer = State#state.player,
	MyEid = MyPlayer#player.eid,
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
							{disconnect, {multiple_login, State#state.player#player.name}, State}; % different atom here?
		
						NewPlayer ->
							Mode = case NewPlayer#player.mode of
								creative -> 1;
								survival -> 0
							end,
							write(Writer, {login_request, [NewPlayer#player.eid, "", "DEFAULT", Mode, 0, 0, 0, 100]}),
							send_inventory(State),
							%debug:
							NewState = update_slot(State#state{player=NewPlayer}, 40, {replace, {3, 10, 0}}),
							write(Writer, {spawn_position, [0, 0, 0]}),
							{X, Y, Z, Yaw, Pitch} = StartPos = State#state.pos,
							
							Chunks = check_chunks(Writer, {X, Y, Z}),
							
							send_player_list(State),							
							write(Writer, {player_position_look, [X,Y+1.62,Y,Z,Yaw,Pitch,1]}),
							
							broadcast_position(StartPos, NewPlayer#player.eid),
							
							mc_erl_chat:broadcast(NewPlayer#player.name ++ " has joined."),
							mc_erl_chat:to_player(NewPlayer#player.name, mc_erl_config:get(motd)),
							NewState#state{chunks=Chunks, logged_in=true}
					end;
				false ->
					io:format("[~s] Someone with the wrong nickname has tried to log in, kicked~n", [?MODULE]),
					write(Writer, {disconnect, ["Invalid username :("]}),
					{disconnect, {invalid_username, MyPlayer#player.name}, State}
			end;
			
		{packet, {keep_alive, [_]}} ->
			State;
			
		{packet, {player, _OnGround}} ->
			State;
			
		{packet, {player_position, [X, Y, _Stance, Z, _OnGround]}} ->
			{_OldX, _OldY, _OldZ, Yaw, Pitch} = State#state.pos,
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, MyPlayer#player.eid),
			
			NewState = State#state{chunks=check_chunks(Writer, {X, Y, Z}, State#state.chunks), pos=NewPos},
			NewState;
			
		{packet, {player_look, [Yaw, Pitch, _OnGround]}} ->
			{X, Y, Z, _OldYaw, _OldPitch} = State#state.pos,
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, MyPlayer#player.eid),
			State#state{pos=NewPos};
			
		{packet, {player_position_look, [X, Y, _Stance, Z, Yaw, Pitch, _OnGround]}} ->
			NewPos = {X, Y, Z, Yaw, Pitch},
			broadcast_position(NewPos, MyPlayer#player.eid),
			State#state{chunks=check_chunks(State#state.writer, {X, Y, Z}, State#state.chunks), pos=NewPos};
		
		{packet, net_disconnect} ->
			{disconnect, {graceful, "Lost connection"}, State};
			
		{packet, {disconnect, [Message]}} ->
			{disconnect, {graceful, Message}, State};
		
		{packet, {holding_change, [N]}} when N >= 0, N =< 8 ->
			NewPlayer = MyPlayer#player{selected_slot=N},
			State#state{player=NewPlayer};
		
		{packet, {player_digging, [0, X, Y, Z, _]}} ->
			case MyPlayer#player.mode of
				creative -> mc_erl_chunk_manager:set_block({X, Y, Z}, {0, 0});
				survival -> void
			end,
			State;
		
		{packet, {player_digging, [2, X, Y, Z, _]}} ->
			case MyPlayer#player.mode of
				creative -> void;
				survival -> mc_erl_chunk_manager:set_block({X, Y, Z}, {0, 0})
			end,
			State;
		
		{packet, {player_block_placement, [-1, -1, -1, -1, {_BlockId, _Count, _Metadata}]}} ->
			% handle held item state update (eating food etc.)
			State;
		
		{packet, {player_block_placement, [_, _, _, _, empty]}} ->
			State;
			
		{packet, {player_block_placement, [X, Y, Z, Direction, {BlockId, _Count, Metadata}]}} when BlockId < 256 ->
			case MyPlayer#player.mode of
				creative ->
					case mc_erl_chunk_manager:set_block({X, Y, Z, Direction}, {BlockId, Metadata}, State#state.pos) of
						ok -> ok;
						{error, forbidden_block_id, {RX, RY, RZ}} ->
							write(Writer, {block_change, [RX, RY, RZ, 0, 0]})
					end,
					State;
				survival ->
					SelectedSlot = MyPlayer#player.selected_slot+36,
					Inv = MyPlayer#player.inventory,
					case array:get(SelectedSlot, Inv) of
						{BlockId, _, Metadata} ->
							mc_erl_chunk_manager:set_block({X, Y, Z, Direction}, {BlockId, Metadata}, State#state.pos),
							update_slot(State, SelectedSlot, reduce);
						_ -> {disconnect, {cheating, wrong_slot}, State}
					end
			end;
		
		{packet, {entity_action, [MyEid, _P]}} ->
			% crouching, leaving bed, sprinting
			State;
		
		{packet, {chat_message, [Message]}} ->
			mc_erl_chat:broadcast(State#state.player, Message),
			State;
		
		{packet, {animation, [MyEid, AnimationId]}} ->
			mc_erl_entity_manager:broadcast_local(MyEid, {animate, MyEid, AnimationId}),
			State;
		
		{packet, {entity_action, [MyEid, 1]}} -> % crouch
			mc_erl_entity_manager:broadcast_local(MyEid, {entity_metadata, MyEid, [{0, {byte, 2}}]}),
			State;
		
		{packet, {entity_action, [MyEid, 2]}} -> % uncrouch
			mc_erl_entity_manager:broadcast_local(MyEid, {entity_metadata, MyEid, [{0, {byte, 0}}]}),
			State;
		
		{packet, {entity_action, [MyEid, _N]}} ->
			% sprinting, leaving bed
			State;
		
		{packet, {player_abilities, [_, _Flying, _, _]}} ->
			State;
		
		{packet, {window_click, [0, Slot, 0, TransactionId, false, _Item]}} -> % player inventory without right-cl and shift
			if
				Slot /= -999 ->
					case State#state.cursor_item of
						empty ->
							Holding = get_slot(State, Slot),
							NewState = update_slot(State, Slot, empty),
							write(Writer, {transaction, [0, TransactionId, true]}),
							NewState#state{cursor_item=Holding};
							
						{BlockId, Count, BlockMetadata} ->
							SlotBelow = get_slot(State, Slot),
							case SlotBelow of
								empty ->
									NewState = update_slot(State, Slot, {replace, State#state.cursor_item}),
									write(Writer, {transaction, [0, TransactionId, true]}),
									NewState#state{cursor_item=empty};
									
								{BelowId, BelowCount, BelowMetadata} ->
									if
										(BelowId == BlockId) andalso (BelowMetadata == BlockMetadata) ->
											if
												Count+BelowCount > 64 ->
													NewBelowCount = 64,
													NewHoldingCount = Count + BelowCount - 64,
													write(Writer, {transaction, [0, TransactionId, true]}),
													NewState=update_slot(State, Slot, {replace, {BelowId, NewBelowCount, BelowMetadata}}),
													NewState#state{cursor_item={BlockId, NewHoldingCount, BlockMetadata}};
												true ->
													NewBelowCount = Count + BelowCount,
													write(Writer, {transaction, [0, TransactionId, true]}),
													NewState=update_slot(State, Slot, {replace, {BelowId, NewBelowCount, BelowMetadata}}),
													NewState#state{cursor_item=empty}
											end;
										
										true ->
											write(Writer, {transaction, [0, TransactionId, false]}),
											State
									end
							end
					end;
				
				true ->
					case State#state.cursor_item of
						empty -> State;
						{_BlockId, _Count, _BlockMetadata} ->
							%todo: spawn dropped item
							State#state{cursor_item=empty}
					end
			end;
		
		{packet, {window_click, [0, Slot, 1, TransactionId, false, _Item]}} -> % right click without shift
			if
				Slot /= -999 ->
					case State#state.cursor_item of
						empty ->
							CSlot = get_slot(State, Slot),
							case CSlot of
								empty -> State;
								{HoldingId, AllCount, HoldingMetadata} ->
									HoldingCount = trunc((AllCount+1)/2),
									LeftCount = trunc(AllCount/2),
									LeftItem = if
										LeftCount == 0 -> empty;
										true -> {HoldingId, LeftCount, HoldingMetadata}
									end,
									NewState = update_slot(State, Slot, {replace, LeftItem}),
									Holding = {HoldingId, HoldingCount, HoldingMetadata},
									write(Writer, {transaction, [0, TransactionId, true]}),
									NewState#state{cursor_item=Holding}
							end;
									
							
						{BlockId, Count, BlockMetadata} ->
							SlotBelow = get_slot(State, Slot),
							case SlotBelow of
								empty ->
									NewCursorCount = Count - 1,
									NewState = update_slot(State, Slot, {replace, {BlockId, 1, BlockMetadata}}),
									write(Writer, {transaction, [0, TransactionId, true]}),
									if
										NewCursorCount == 0 -> NewCItem = empty;
										true -> NewCItem = {BlockId, NewCursorCount, BlockMetadata}
									end,
									NewState#state{cursor_item=NewCItem};
									
								{BelowId, BelowCount, BelowMetadata} ->
									if
										(BelowId == BlockId) andalso (BelowMetadata == BlockMetadata) ->
											if
												BelowCount+1 > 64 ->
													write(Writer, {transaction, [0, TransactionId, false]}),
													State;
												true ->
													NewCursorCount = Count - 1,
													if
														NewCursorCount == 0 -> NewCItem = empty;
														true -> NewCItem = {BlockId, NewCursorCount, BlockMetadata}
													end,
													NewBelowCount = BelowCount + 1,
													write(Writer, {transaction, [0, TransactionId, true]}),
													NewState=update_slot(State, Slot, {replace, {BelowId, NewBelowCount, BelowMetadata}}),
													NewState#state{cursor_item=NewCItem}
											end;
										
										true ->
											write(Writer, {transaction, [0, TransactionId, false]}),
											State
									end
							end
					end;
				
				true ->
					case State#state.cursor_item of
						empty -> State;
						{BlockId, Count, BlockMetadata} ->
							%todo: spawn dropped item
							if
								(Count - 1) == 0 -> State#state{cursor_item=empty};
								true -> State#state{cursor_item={BlockId, Count - 1, BlockMetadata}}
							end
					end
			end;
		
		{packet, {window_click, [_, _, _, TransactionId, _, _]}} ->
			write(Writer, {transaction, [0, TransactionId, false]}),
			State;
		
		{packet, UnknownPacket} ->
			io:format("[~s] unhandled packet: ~p~n", [?MODULE, UnknownPacket]),
			State;
		% protocol reactions end
		
		% chat
		{chat, Message} ->
			write(Writer, {chat_message, [Message]}),
			State;
		
		{animate, Eid, AnimationId} ->
			case dict:is_key(Eid, State#state.known_entities) of
				true -> write(Writer, {animation, [Eid, AnimationId]});
				false -> ok
			end,
			State;
		
		{entity_metadata, Eid, Metadata} ->
			case dict:is_key(Eid, State#state.known_entities) of
				true -> write(Writer, {entity_metadata, [Eid, Metadata]});
				false -> ok
			end,
			State;
			
		{tick, Tick} ->
			if
				(Tick rem 20) == 0 ->
					write(Writer, {time_update, [Tick]});
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
		
		% just a notification, player_logic pulls column if necessary
		% possible enhancement: when compressed columns are cached, chunk_manager can send compressed chunk (hence binaries are referenced!)
		{update_column, {X, Z}=Coord} ->
			case sets:is_element(Coord, State#state.chunks) of
				false -> ok;
				true ->
					ChunkData = mc_erl_chunk_manager:get_chunk(Coord),
					write(Writer, {map_chunk, [X, Z, {parsed, ChunkData}]})
			end,
			State;
		
		{new_player, Player} ->
			write(Writer, {player_list_item, [Player#player.name, true, 1]}),
			State;
		
		{delete_player, Player} ->
			write(Writer, {player_list_item, [Player#player.name, false, 1]}),
			NewState = delete_entity(Player#player.eid, State),
			NewState;
		
		{delete_entity, Eid} ->
			NewState = delete_entity(Eid, State),
			NewState;
				
		
		{update_entity_position, {Eid, {_X, _Y, _Z, _Yaw, _Pitch}=NewLocation}} ->
			NewState = update_entity(Eid, NewLocation, State),
			NewState;
		
		net_disconnect ->
			{disconnect, net_disconnect, State};
		
		UnknownMessage ->
			io:format("[~s] unknown message: ~p~n", [?MODULE, UnknownMessage]),
			State
	end,
	case RetState of
		% graceful stops
		{disconnect, net_disconnect, DisconnectState} ->
			io:format("[~s] Connection lost with ~s~n", [?MODULE, DisconnectState#state.player#player.name]),
			{stop, normal, DisconnectState};

		{disconnect, {graceful, _QuitMessage}, DisconnectState} ->
			io:format("[~s] Player ~s has quit~n", [?MODULE, DisconnectState#state.player#player.name]),
			{stop, normal, DisconnectState};
		
		% not graceful stops
		{disconnect, {invalid_username, AttemptedName}, DisconnectState} ->
			io:format("[~s] Invalid username trying to log in: ~s~n", [?MODULE, AttemptedName]),
			{stop, normal, DisconnectState};

		{disconnect, {multiple_login, AttemptedName}, DisconnectState} ->
			io:format("[~s] Multiple login: ~s~n", [?MODULE, AttemptedName]),
			{stop, normal, DisconnectState};
		
		{disconnect, {cheating, Reason}, DisconnectState} ->
			io:format("[~s] player is kicked due cheating: ~p~n", [?MODULE, Reason]),
			{stop, normal, DisconnectState};

		{disconnect, Reason, DisconnectState} -> {stop, Reason, DisconnectState};
		
		% right path
		Res -> {noreply, Res}
	end.

% ==== update entity location
update_entity(Eid, {X, Y, Z, _Yaw, _Pitch}=NewLocation, State) ->
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
					true -> move_known_entity(Eid, NewLocation, State);
					false -> spawn_new_entity(Eid, NewLocation, State)
				end
		end
	end.

write(Writer, Packet) -> Writer ! {packet, Packet}.


move_known_entity(Eid, {X, Y, Z, Yaw, Pitch}, State) ->
	Writer = State#state.writer,
	{OldX, OldY, OldZ, _OldYaw, _OldPitch, KEMetadata} = dict:fetch(Eid, State#state.known_entities),
	RelativeRelocations = KEMetadata#ke_metadata.relocations,
	DX = X - OldX,
	DY = Y - OldY,
	DZ = Z - OldZ,
	DDistance = lists:max([DX, DY, DZ]),
	FracYaw = trunc(Yaw*256/360),
	FracPitch = trunc(Pitch*256/360),
	
	ChangePackets = if
		(DDistance >= 4) or (RelativeRelocations >= 20) ->
			NewKnownEntities = dict:store(Eid, {X, Y, Z, Yaw, Pitch, KEMetadata#ke_metadata{relocations=0}}, State#state.known_entities),
			[{entity_teleport, [Eid, X, Y, Z, FracYaw, FracPitch]}];
		true ->
			NewKnownEntities = dict:store(Eid, {X, Y, Z, Yaw, Pitch, KEMetadata#ke_metadata{relocations=RelativeRelocations + 1}}, State#state.known_entities),
			[{entity_look_move, [Eid, DX, DY, DZ, FracYaw, FracPitch]},
			 {entity_head_look, [Eid, FracYaw]}]
	end,
	lists:map(fun(Packet) -> write(Writer, Packet) end, ChangePackets),
	State#state{known_entities=NewKnownEntities}.

spawn_new_entity(Eid, {X, Y, Z, Yaw, Pitch}, State) ->
	Writer = State#state.writer,
	EntityData = mc_erl_entity_manager:entity_details(Eid),
	if
		EntityData#entity_data.type == player ->
			PName = EntityData#entity_data.metadata#player_metadata.name,
			PHolding = EntityData#entity_data.metadata#player_metadata.holding_item,
			write(Writer, {named_entity_spawn, [Eid, PName, X, Y, Z, trunc(Yaw*256/360), trunc(Yaw*256/360), PHolding]}),
			NewKnownEntities = dict:store(Eid, {X, Y, Z, Yaw, Pitch, #ke_metadata{}}, State#state.known_entities),
			State#state{known_entities=NewKnownEntities};
		true ->
			State
	end.

delete_entity(Eid, State) ->
	case dict:is_key(Eid, State#state.known_entities) of
		true ->	
			write(State#state.writer, {destroy_entity, [Eid]}),
			NewKnownEntities = dict:erase(Eid, State#state.known_entities),
			State#state{known_entities=NewKnownEntities};
		false ->
			State
	end.

send_player_list(State) ->
	Writer = State#state.writer,
	Players = mc_erl_entity_manager:get_all_players(),
	lists:foreach(fun(Player) -> write(Writer, {player_list_item, [Player#player.name, true, 1]}) end, Players).

send_inventory(State) ->
	Writer = State#state.writer,
	write(Writer, {window_items, [0, array:to_list(State#state.player#player.inventory)]}).

%% action = reduce | {replace, Slot} | empty
update_slot(State, SlotNo, Action) ->
	Writer = State#state.writer,
	Player = State#state.player,
	Inv = Player#player.inventory,
	NewSlot = case Action of
		empty ->
			empty;
		reduce ->
			case array:get(SlotNo, Inv) of
				empty -> empty;
				{_BlockId, 1, _Metadata} -> empty;
				{BlockId, Count, Metadata} -> {BlockId, Count-1, Metadata}
			end;
		{replace, Slot} -> Slot
	end,
	NewInv = array:set(SlotNo, NewSlot, Inv),
	write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
	State#state{player=Player#player{inventory=NewInv}}.

get_slot(State, SlotNo) -> array:get(SlotNo, State#state.player#player.inventory).

% ==== Send position to everyone
broadcast_position(Position, Eid) ->
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

