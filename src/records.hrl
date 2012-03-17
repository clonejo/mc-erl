% entities:
-record(player, {eid, name, player_logic}).

-record(chunk_column_data, {full_column, types, metadata,
                            block_light, sky_light, add_data=[], biome}).
