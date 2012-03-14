-modulue(mc_erl_chunk_generator).

-export([gen_chunk/1]).

-include("records.hrl").

gen_column({chunk, X, Y}) ->
	#chunk_column_data(full_column=true,
	                   types=[{0, binary:copy(<<7>>,256)
	                              ++ binary:copy(<<0>>,256*15)}],
	                   metadata=[{0, binary:copy(<<0>>,16*16*16/2)}],
	                   block_light=[{0, binary:copy(<<255>>,16*16*16/2)}],
	                   sky_light=[{0, binary:copy(<<255>>,16*16*16/2)}],
	                   biome=binary:copy(<<0>>,256)).
