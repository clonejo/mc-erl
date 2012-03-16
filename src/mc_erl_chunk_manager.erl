-module(mc_erl_chunk_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0, coord_to_chunk/1, chunks_in_range/2, get_chunk/1]).

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

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

%% returns a set of {chunk, X, Z} coordinates
chunks_in_range({_, _, _}=Pos, Range) ->
	chunks_in_range(coord_to_chunk(Pos), Range);
chunks_in_range({CX, CZ}, Range) ->
	sets:from_list(lists:flatten(
		[[{X, Z} || X<-lists:seq(CX-Range, CX+Range)]||
			Z<-lists:seq(CZ-Range, CZ+Range)])).

asynchronous_get_chunk(ChunkCoord, Chunks) ->
	Chunk = case ets:lookup(Chunks, ChunkCoord) of
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


