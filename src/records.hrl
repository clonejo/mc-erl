% entities:
-record(player, {eid, name, player_logic}).

-record(chunk_column_data, {full_column, chunks=[], add_data=[], biome}).
-record(chunk_data, {types, metadata, block_light, sky_light}).
