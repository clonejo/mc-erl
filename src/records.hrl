% entities:
-record(player, {eid, name, player_logic, mode=survival,
                 inventory=array:new(45, {default, empty}), selected_slot=0}).
-record(entity_data, {eid, type, location={0,0,0,0,0}, metadata}).

-record(player_metadata, {name, holding_item}).

%blocks/chunks:
-record(block_type, {id, name, placeable=false}).

-record(chunk_column_data, {full_column, chunks=[], add_data=[], biome}).
-record(chunk_data, {types, metadata, block_light, sky_light}).
