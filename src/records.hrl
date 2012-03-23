% entities:
-record(player, {eid, name, player_logic}).
-record(entity_data, {eid, type, location={0,0,0,0,0}, metadata}).

-record(player_metadata, {name, holding_item}).

-record(block_type, {id, name, placeable=false}).

-record(chunk_column_data, {full_column, chunks=[], add_data=[], biome}).
-record(chunk_data, {types, metadata, block_light, sky_light}).
