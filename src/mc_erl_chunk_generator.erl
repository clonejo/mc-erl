%% @copyright 2012-2013 Feiko Nanninga

-module(mc_erl_chunk_generator).

-export([gen_column/1]).

-include("records.hrl").

gen_column({X, Y}) ->
    io:format("[~w] generated chunk ~p~n", [?MODULE, {chunk, X, Y}]),
    EmptyChunk = #chunk_data{types=binary:copy(<<0>>,16*16*16),
                             metadata=binary:copy(<<0>>,16*16*8),
                             block_light=binary:copy(<<255>>,16*16*8),
                             sky_light=binary:copy(<<255>>,16*16*8)},
    BedrockChunk = EmptyChunk#chunk_data{types=list_to_binary([binary:copy(<<7>>,256),
                                                               binary:copy(<<1>>,256*15)])},
    StoneChunk = EmptyChunk#chunk_data{types=binary:copy(<<1>>,16*16*16)},
    TopChunk = EmptyChunk#chunk_data{types=list_to_binary([binary:copy(<<3>>,256*15),
                                                           binary:copy(<<2>>,256)])},
    #chunk_column_data{full_column=true,
                       chunks=[{0, BedrockChunk},
                               {1, StoneChunk},
                               {2, StoneChunk},
                               {3, TopChunk},
                               {4, EmptyChunk}],
                       biome=binary:copy(<<0>>,256)}.
