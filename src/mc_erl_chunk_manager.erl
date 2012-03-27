-module(mc_erl_chunk_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0, clear_map/0, coord_to_chunk/1, chunks_in_range/2, get_chunk/1,
         set_block/2, set_block/3, loaded_chunks/0, undirectional_block_coord/1]).

-include("records.hrl").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

%% converts coordinates to chunk coordinates
coord_to_chunk({X, _Y, Z}) ->
	{floor(X/16), floor(Z/16)}.

coord_within_chunk({X, Y, Z}) ->
	{X - floor(X/16)*16, Y - floor(Y/16)*16, Z - floor(Z/16)*16}.

floor(X) when X < 0 ->
	T = trunc(X),
	case X - T == 0 of
		true -> T;
		false -> T - 1
	end;
floor(X) ->
	trunc(X).

undirectional_block_coord({X, Y, Z, Direction}) ->
	case Direction of
		0 -> {X, Y-1, Z};
		1 -> {X, Y+1, Z};
		2 -> {X, Y, Z-1};
		3 -> {X, Y, Z+1};
		4 -> {X-1, Y, Z};
		5 -> {X+1, Y, Z}
	end.

%% returns a set of {chunk, X, Z} coordinates
chunks_in_range({_, _, _}=Pos, Range) ->
	chunks_in_range(coord_to_chunk(Pos), Range);
chunks_in_range({CX, CZ}, Range) ->
	sets:from_list(lists:flatten(
		[[{X, Z} || X<-lists:seq(CX-Range, CX+Range)]||
			Z<-lists:seq(CZ-Range, CZ+Range)])).

asynchronous_get_chunk(ChunkCoord, Chunks) ->
	case ets:lookup(Chunks, ChunkCoord) of
		[] ->
			C = mc_erl_chunk_generator:gen_column(ChunkCoord),
			ets:insert(Chunks, {ChunkCoord, C}),
			C;
		[{ChunkCoord, C}] -> C
	end.
			
get_chunk({_, _, _}=Pos) ->
	get_chunk(coord_to_chunk(Pos));
get_chunk({_, _}=Coord) ->
	gen_server:call(?MODULE, {get_chunk, Coord}).

loaded_chunks() -> gen_server:call(?MODULE, loaded_chunks).

clear_map() -> gen_server:cast(?MODULE, clear_map).

%get_compressed_chunk(...

orientation(Yaw) ->
	Sin = math:sin((Yaw+45)/360*2*math:pi()),
	Cos = math:cos((Yaw+45)/360*2*math:pi()),
	if Sin =< 0 ->
		if Cos =< 0 ->
			north;
		true ->
			east
		end;
	true ->
		if Cos =< 0 ->
			west;
		true ->
			south
		end
	end.
			

set_block({_, _, _, Direction}=C, {BlockId, Metadata}, {_, _, _, Yaw, _}) ->
	{X, Y, Z} = BlockCoord = undirectional_block_coord(C),
	case mc_erl_blocks:can_build(BlockId) of
		true ->
			NewMetadata = if
				BlockId =:= 50 orelse BlockId =:= 75 orelse BlockId =:= 76 ->
					case Direction of
						1 -> 5;
						2 -> 4;
						3 -> 3;
						4 -> 2;
						5 -> 1;
						_ -> 0
					end;
				BlockId =:= 53 orelse BlockId =:= 67 orelse BlockId =:= 67
				               orelse BlockId =:= 108 orelse BlockId =:= 109
				               orelse BlockId =:= 114 ->
					case orientation(Yaw) of
						east -> 0;
						west -> 1;
						south -> 2;
						north -> 3
					end;
				true -> Metadata
			end,
			mc_erl_entity_manager:broadcast_visible({X, Y, Z}, {block_delta, {X, Y, Z, BlockId, NewMetadata}}),
			set_block(BlockCoord, {BlockId, NewMetadata});
		false ->
			{error, forbidden_block_id, BlockCoord}
	end.

set_block({X, Y, Z}=BlockCoord, {BlockId, Metadata}=BlockData) ->
	mc_erl_entity_manager:broadcast_visible(BlockCoord, {block_delta, {X, Y, Z, BlockId, Metadata}}),
	gen_server:cast(?MODULE, {set_block, BlockCoord, BlockData}).

% gen_server callbacks
init([]) ->
	io:format("[~s] starting~n", [?MODULE]),
	Chunks = ets:new(chunks, [set, public]),
	{ok, Chunks}.

handle_call({get_chunk, ChunkCoord}, From, Chunks) ->
	proc_lib:spawn_link(fun() ->
		Chunk = asynchronous_get_chunk(ChunkCoord, Chunks),
		gen_server:reply(From, Chunk)
		end),
	{noreply, Chunks};

handle_call(loaded_chunks, _From, Chunks) ->
	{reply, ets:info(Chunks, size), Chunks};

handle_call(Message, _From, State) ->
	case Message of
		_ ->
			io:format("[~s] received call: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.


handle_cast(clear_map, Chunks) ->
	ets:delete(Chunks),
	{noreply, ets:new(chunks, [set, public])};

handle_cast({set_block, {_X, Y, _Z}=Coord, {BlockId, Metadata}}, Chunks) ->
	Column = asynchronous_get_chunk(coord_to_chunk(Coord), Chunks),
	Chunk = case proplists:get_value(Y div 16, Column#chunk_column_data.chunks) of
		undefined -> #chunk_data{types=binary:copy(<<0>>,16*16*16),
	                             metadata=binary:copy(<<0>>,16*16*8),
	                             block_light=binary:copy(<<255>>,16*16*8),
	                             sky_light=binary:copy(<<255>>,16*16*8)};
		C -> C
	end,
	
	% changing types
	{RX, RY, RZ} = coord_within_chunk(Coord),
	ByteOffset = RX+RZ*16+RY*256,
	{Head, Rest} = split_binary(Chunk#chunk_data.types, ByteOffset),
	{_, Tail} = split_binary(Rest, 1),
	NewTypes = list_to_binary([Head, BlockId, Tail]),
	
	% changing metadata value
	NibbleOffset = floor(ByteOffset/2),
	{MetaHead, MetaRest} = split_binary(Chunk#chunk_data.metadata, NibbleOffset),
	{<<M1:4, M2:4>>, MetaTail} = split_binary(MetaRest, 1), % i hate nibble packing
	NewMetadataValue = case ByteOffset rem 2 of
		0 -> <<M1:4, Metadata:4>>;
		1 -> <<Metadata:4, M2:4>>
	end,
	NewMetadata = list_to_binary([MetaHead, NewMetadataValue, MetaTail]),
	
	NewChunk = Chunk#chunk_data{types=NewTypes, metadata=NewMetadata},
	NewColumn = Column#chunk_column_data{
		chunks=lists:keystore(Y div 16, 1, Column#chunk_column_data.chunks,
		                     {Y div 16, NewChunk})},
	
	ets:insert(Chunks, {coord_to_chunk(Coord), NewColumn}),
	{noreply, Chunks};

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


