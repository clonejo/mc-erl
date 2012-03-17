-module(mc_erl_player_logic).
% only pure erlang, only pure hardcore
-export([init_logic/2]).
-record(logic_state, {writer, name="Player", eid=0, chunks=none, 
	entity_details=dict:new(), 
	x=0, y=0, z=0}).

init_logic(Writer, Name) ->
	loop(#logic_state{writer=Writer, name=Name}).

write(Writer, Packet) -> Writer ! {packet, Packet}.

loop(State) ->
	Writer = State#logic_state.writer,
	receive
		login_sequence ->
			write(Writer, {login_request, [100, "", "DEFAULT", 1, 0, 0, 0, 100]}),
			write(Writer, {spawn_position, [0, 0, 0]}),
			Chunks = check_chunks(Writer, {0, 0}),
			write(Writer, {player_position_look, [0,6,5,0,0,0,1]}),
			loop(State#logic_state{chunks=Chunks});
			
		{packet, {keep_alive, [_]}} ->
			loop(State);
			
		{packet, {player, OnGround}} ->
			loop(State);
			
		{packet, {player_position, [X, Y, Stance, Z, OnGround]}} ->
			NewState = State#logic_state{chunks=check_chunks(State#logic_state.writer, {X, Y, Z}, State#logic_state.chunks)},
			loop(NewState);
			
		{packet, {player_look, [Yaw, Pitch, OnGround]}} ->
			loop(State);
			
		{packet, {player_position_look, [X, Y, Stance, Z, Yaw, Pitch, OnGround]}} ->
			NewState = State#logic_state{chunks=check_chunks(State#logic_state.writer, {X, Y, Z}, State#logic_state.chunks)},
			loop(NewState);
			
		{packet, {disconnect, [Message]}} ->
			io:format("[~s] A player disconnected: \"~s\"~n", [?MODULE, Message]),
			exit(disconnect);
		
		{packet, {player_digging, [0, X, Y, Z, _]}} ->
				mc_erl_chunk_manager:set_block({X, Y, Z}, {0, 0}),
				loop(State);
					
		{packet, {player_block_placement, [X, Y, Z, Direction, {BlockId, 1, Metadata}]}} ->
				mc_erl_chunk_manager:set_block({X, Y, Z, Direction}, {BlockId, Metadata}),
				loop(State);
			
		{packet, UnknownPacket} ->
			io:format("[~s] unhandled packet: ~p~n", [?MODULE, UnknownPacket]),
			loop(State)
		
		% {update_entity, Eid, {X, Y, Z, Yaw, Pitch}=NewPosition} ->
			% case find(Eid, State#logic_state.entities) of
				% {ok, {player, PName}} -> 
	end.
		

		
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

