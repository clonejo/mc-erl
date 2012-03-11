-module(mc_erl_packets).
-export([get_by_id/1, get_by_name/1]).

get_by_id(Id) ->
	case Id of
		0 ->
			{0, keep_alive, [int]};
		1 ->
			{1, login_request, [int, string, string, int, int,
			                    byte, ubyte, ubyte]};
		2 ->
			{2, handshake, [string]};
		3 ->
			{3, chat_message, [string]};
		4 ->
			{4, time_update, [long]};
		5 ->
			{5, entity_equipment, [int, short, short, short]};
		6 ->
			{6, spawn_position, [int, int, int]};
		7 ->
			{7, use_entity, [int, int, bool]};
		8 ->
			{8, update_health, [short, short, float]};
		9 ->
			{9, respawn, [int, byte, byte, short, string]};
		10 ->
			{10, player, [bool]};
		11 ->
			{11, player_position, [double, double, double, double, bool]};
		12 ->
			{12, player_look, [float, float, bool]};
		13 ->
			{13, player_position_look, [double, double, double, double,
			                            float, float, bool]};
		14 ->
			{14, player_digging, [byte, int, byte, int, byte]};
		15 ->
			{15, player_block_placement, [int, byte, int, byte, slot]};
		16 ->
			{16, holding_change, [short]};
		17 ->
			{17, use_bed, [int, byte, int, byte, int]};
		18 ->
			{18, animation, [int, byte]};
		19 ->
			{19, entity_action, [int, byte]};
		20 ->
			{20, named_entity_spawn, [int, string, int, int, int,
			                          byte, byte, short]};
		21 ->
			{21, pickup_spawn, [int, short, byte, short,
			                    int, int, int, byte, byte, byte]};
		22 ->
			{22, collect_item, [int, int]};
		23 -> % no fireballs supported!
			{23, add_object, [int, byte, int, int, int, int]}; 
		24 ->
			{24, mob_spawn, [int, byte, int, int, int, byte, byte, byte, metadata]};
		26 ->
			{26, experience_orb, [int, int, int, int, short]};
		28 ->
			{28, entity_velocity, [int, short, short, short]};
		29 ->
			{29, destroy_entity, [int]};
		31 ->
			{31, entity_move, [int, byte, byte, byte]};
		32 ->
			{32, entity_look, [int, byte, byte]};
		33 ->
			{33, entity_look_move, [int, byte, byte, byte, byte, byte]};
		34 ->
			{34, entity_teleport, [int, int, int, int, byte, byte]};
		35 ->
			{35, entity_head_look, [int, byte]};
		38 ->
			{38, entity_status, [int, byte]};
		39 ->
			{39, attach_entity, [int, int]};
		40 ->
			{40, entity_metadata, [int, metadata]};
		41 ->
			{41, entity_effect, [int, byte, byte, short]};
		42 ->
			{42, remove_entity_effect, [int, byte]};
		43 ->
			{43, experience, [float, short, short]};
		50 ->
			{50, pre_chunk, [int, int, bool]};
		51 ->
			{51, map_chunk, [int, int, bool, ushort, ushort, chunk_data]};
		52 ->
			{52, multi_block_change, [int, int, multi_block_change_data]};
		53 ->
			{53, block_change, [int, byte, int, byte, byte]};
		54 ->
			{54, block_action, [int, short, int, byte, byte]};
		60 ->
			{60, explosion, [double, double, double, float, coordinate_offsets]};
		61 ->
			{61, sound_particle_effect, [int, int, byte, int, int]};
		70 ->
			{70, new_invalid_state, [byte, byte]};
		71 ->
			{71, thunderbolt, [int, bool, int, int, int]};
		100 ->
			{100, open_window, [byte, byte, string, byte]};
		101 ->
			{101, close_window, [byte]};
		102 ->
			{102, window_click, [byte, short, byte, short, bool, slot]};
		103 ->
			{103, set_slot, [byte, short, slot]};
		104 ->
			{104, window_items, [byte, slots]};
		105 ->
			{105, update_window_property, [byte, short, short]};
		106 ->
			{106, transaction, [byte, short, bool]};
		107 ->
			{107, creative_inventory_action, [short, slot]};
		108 ->
			{108, enchant_item, [byte, byte]};
		132 ->
			{132, update_tile_entity, [int, short, int, byte, int, int, int]};
		200 ->
			{200, increment_statistic, [int, byte]};
		201 ->
			{201, player_list_item, [string, bool, short]};
		254 ->
			{254, server_list_ping, []};
		255 ->
			{255, disconnect, [string]};
		X ->
			{error, {unknown_id, X}}
	end.

get_by_name(Name) ->
	get_by_id(case Name of
		keep_alive -> 0;
		login_request -> 1;
		handshake -> 2;
		chat_message -> 3;
		time_update -> 4;
		entity_equipment -> 5;
		spawn_position -> 6;
		use_entity -> 7;
		update_health -> 8;
		respawn -> 9;
		player -> 10;
		player_position -> 11;
		player_look -> 12;
		player_position_look -> 13;
		player_digging -> 14;
		player_block_placement -> 15;
		holding_change -> 16;
		use_bed -> 17;
		animation -> 18;
		entity_action -> 19;
		named_entity_spawn -> 20;
		pickup_spawn -> 21;
		collect_item -> 22;
		add_object -> 23;
		mob_spawn -> 24;
		experience_orb -> 26;
		entity_velocity -> 28;
		destroy_entity -> 29;
		entity_move -> 31;
		entity_look -> 32;
		entity_look_move -> 33;
		entity_teleport -> 34;
		entity_head_look -> 35;
		entity_status -> 38;
		attach_entity -> 39;
		entity_metadata -> 40;
		entity_effect -> 41;
		remove_entity_effect -> 42;
		experience -> 43;
		pre_chunk -> 50;
		map_chunk -> 51;
		multi_block_change -> 52;
		block_change -> 53;
		block_action -> 54;
		explosion -> 60;
		sound_particle_effect -> 61;
		new_invalid_state -> 70;
		thunderbolt -> 71;
		open_window -> 100;
		close_window -> 101;
		window_click -> 102;
		set_slot -> 103;
		window_items -> 104;
		update_window_property -> 105;
		transaction -> 106;
		creative_inventory_action -> 107;
		enchant_item -> 108;
		update_tile_entity -> 132;
		increment_statistic -> 200;
		player_list_item -> 201;
		server_list_ping -> 254;
		disconnect -> 255;
		X ->
			{error, {unknown_name, X}}
	end).
