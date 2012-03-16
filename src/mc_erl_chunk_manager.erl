-module(mc_erl_chunk_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0, coord_to_chunk/1, chunks_in_range/2, get_chunk/1, set_block/2]).

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

block_coord({X, Y, Z, Direction}) ->
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

%get_compressed_chunk(...

set_block({_, _, _, _}=C, {_, _}=BlockData) ->
	BlockCoord = block_coord(C),
	set_block(BlockCoord, BlockData);
set_block({_, _, _}=BlockCoord, {_, _}=BlockData) ->
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

handle_call(Message, _From, State) ->
	case Message of
		_ ->
			io:format("[~s] received call: ~p~n", [?MODULE, Message]),
			{noreply, State}
	end.

%% metadata is currently ignored
handle_cast({set_block, {_X, Y, _Z}=Coord, {BlockId, Metadata}}, Chunks) ->
	Column = asynchronous_get_chunk(coord_to_chunk(Coord), Chunks),
	Types = Column#chunk_column_data.types,
	Chunk = case proplists:get_value(Y div 16, Types) of
		undefined -> binary:copy(<<0>>, 256);
		C -> C
	end,
	{RX, RY, RZ} = coord_within_chunk(Coord),
	{Head, Rest} = split_binary(Chunk, RX+RZ*16+RY*256),
	{_, Tail} = split_binary(Rest, 1),
	NewChunk = list_to_binary([Head, BlockId, Tail]),
	NewTypes = lists:keyreplace(Y div 16, 1, Types, {Y div 16, NewChunk}),
	NewColumn = Column#chunk_column_data{types=NewTypes},
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


