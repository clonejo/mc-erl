% entities:
-record(client, {entity_id, name, recv_process, socket}).

-record(chunk_column_data, {full_column, types, metadata,
                            block_light, sky_light, add_data, biome}).
