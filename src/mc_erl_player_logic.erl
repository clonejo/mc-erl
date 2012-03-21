-module(mc_erl_player_logic).
% only pure erlang, only pure hardcore
-export([init_logic/2]).
-record(state, {writer, player, eid, chunks=none, 
                entity_details=dict:new(), pos={0.5, 70, 0.5}}).

-include("records.hrl").

init_logic(Writer, Name) ->
	loop(#state{writer=Writer, player=#player{name=Name, player_logic=self()}}).

write(Writer, Packet) -> Writer ! {packet, Packet}.

loop(State) ->
	Writer = State#state.writer,
	receive
		login_sequence ->
			case mc_erl_entity_manager:register_player(State#state.player) of
				{error, name_in_use} ->
					io:format("[~s] Someone with the same name is already logged in, kicked~n", [?MODULE]),
					write(Writer, {disconnect, ["Someone with the same name is already logged in :("]}),
					exit(disconnect); % different atom here?
				NewPlayer ->
					write(Writer, {login_request, [NewPlayer#player.eid, "", "DEFAULT", 1, 0, 0, 0, 100]}),
					write(Writer, {spawn_position, [0, 0, 0]}),
					Chunks = check_chunks(Writer, State#state.pos),
					{X, Y, Z} = State#state.pos,
					write(Writer, {player_position_look, [X,Y+1.62,Y,Z,0,0,1]}),
					loop(State#state{chunks=Chunks, player=NewPlayer})
			end;
			
		{packet, {keep_alive, [_]}} ->
			loop(State);
			
		{packet, {player, _OnGround}} ->
			loop(State);
			
		{packet, {player_position, [X, Y, _Stance, Z, _OnGround]}} ->
			NewState = State#state{chunks=check_chunks(State#state.writer, {X, Y, Z}, State#state.chunks)},
			loop(NewState);
			
		{packet, {player_look, [_Yaw, _Pitch, _OnGround]}} ->
			loop(State);
			
		{packet, {player_position_look, [X, Y, _Stance, Z, _Yaw, _Pitch, _OnGround]}} ->
			NewState = State#state{chunks=check_chunks(State#state.writer, {X, Y, Z}, State#state.chunks)},
			loop(NewState);
			
		{packet, {disconnect, [Message]}} ->
			io:format("[~s] A player disconnected: \"~s\"~n", [?MODULE, Message]),
			mc_erl_entity_manager:delete_player(State#state.player),
			exit(disconnect);
		
		{packet, {player_digging, [0, X, Y, Z, _]}} ->
			mc_erl_chunk_manager:set_block({X, Y, Z}, {0, 0}),
			loop(State);
		
		{packet, {player_block_placement, [-1, -1, -1, -1, {_BlockId, 1, _Metadata}]}} ->
			% handle held item state update (eating food etc.)
			loop(State);
			
		{packet, {player_block_placement, [X, Y, Z, Direction, {BlockId, 1, Metadata}]}} when BlockId < 256 ->
			mc_erl_chunk_manager:set_block({X, Y, Z, Direction}, {BlockId, Metadata}),
			loop(State);
		
		{chat, Message} ->
			write(Writer, {chat_message, [Message]});
		
		{packet, UnknownPacket} ->
			io:format("[~s] unhandled packet: ~p~n", [?MODULE, UnknownPacket]),
			loop(State)
		
		% {update_entity, Eid, {X, Y, Z, Yaw, Pitch}=NewPosition} ->
			% case find(Eid, State#state.entities) of
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

