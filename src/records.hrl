
% ======================================================================
% entities
% ======================================================================

%% used for (RAM) entity table
%% eid: [key]
%% name: only used for players [index]
%% type: player|mob|drop|falling_block
%% item_id: hold item for players/mobs, item id for drops
-record(entity, {eid, name, type, logic, location={0,0,0,0,0}, item_id = empty}).

%% used for persistent player table and within player_logic
-record(player, {eid, name, mode=survival, location={0,0,0,0,0},
                 inventory=array:new(45, {default, empty}), selected_slot=0}).

% ======================================================================
% blocks/chunks
% ======================================================================

-record(block_type, {id, name, placeable=false}).

-record(chunk_column_data, {full_column, chunks=[], add_data=[], biome}).
-record(chunk_data, {types, metadata, block_light, sky_light}).
