-module(mc_erl_chunk_generator).

-export([gen_column/1]).

-include("records.hrl").

gen_column({X, Y}) ->
	io:format("[~w] generated chunk ~p~n", [?MODULE, {chunk, X, Y}]),
	#chunk_column_data{full_column=true,
	                   types=[{0, list_to_binary([binary:copy(<<7>>,256),
	                                             binary:copy(<<1>>,256*15)])},
	                          {1, binary:copy(<<1>>,16*16*16)},
	                          {2, binary:copy(<<1>>,16*16*16)},
	                          {3, list_to_binary([binary:copy(<<3>>,256*15),
	                                             binary:copy(<<2>>,256)])}],
	                   metadata=[{0, binary:copy(<<0>>,16*16*8)},
	                             {1, binary:copy(<<0>>,16*16*8)},
	                             {2, binary:copy(<<0>>,16*16*8)},
	                             {3, binary:copy(<<0>>,16*16*8)}],
	                   block_light=[{0, binary:copy(<<255>>,16*16*8)},
	                                {1, binary:copy(<<255>>,16*16*8)},
	                                {2, binary:copy(<<255>>,16*16*8)},
	                                {3, binary:copy(<<255>>,16*16*8)}],
	                   sky_light=[{0, binary:copy(<<255>>,16*16*8)},
	                              {1, binary:copy(<<255>>,16*16*8)},
	                              {2, binary:copy(<<255>>,16*16*8)},
	                              {3, binary:copy(<<255>>,16*16*8)}],
	                   biome=binary:copy(<<0>>,256)}.
