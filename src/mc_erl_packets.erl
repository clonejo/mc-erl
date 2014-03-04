%% @copyright 2012-2013 Gregory Fefelov, Feiko Nanninga

-module(mc_erl_packets).
-export([get_by_id/1, get_by_name/1, get_enchantment_by_id/1, get_enchantment_by_name/1]).

get_by_id(Id) ->
	case Id of
		0 ->
			{0, keep_alive, [int]};
		1 ->
			{1, login_request, [int, string, byte, byte, byte, ubyte, ubyte]};
		2 ->
			{2, handshake, [byte, string, string, int]};
		3 ->
			{3, chat_message, [string]};
		4 ->
			{4, time_update, [long, long]};
		5 ->
			{5, entity_equipment, [int, short, slot]};
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
			{15, player_block_placement, [int, ubyte, int, byte, slot, byte, byte, byte]};
		16 ->
			{16, holding_change, [short]};
		17 ->
			{17, use_bed, [int, byte, int, byte, int]};
		18 ->
			{18, animation, [int, byte]};
		19 ->
			{19, entity_action, [int, byte]};
		20 ->
			{20, named_entity_spawn, [int, string, abs_int, abs_int, abs_int,
			                          byte, byte, short, metadata]};
		22 ->
			{22, collect_item, [int, int]};
		23 -> % no fireballs supported!
			{23, add_object, [int, byte, abs_int, abs_int, abs_int, byte, byte, projectile_data]}; 
		24 ->
			{24, mob_spawn, [int, byte, abs_int, abs_int, abs_int, byte, byte, byte, short, short, short, metadata]};
		26 ->
			{26, experience_orb, [int, int, int, int, short]};
		28 ->
			{28, entity_velocity, [int, short, short, short]};
		29 ->
			{29, destroy_entity, [{array, byte, int}]};
		31 ->
			{31, entity_move, [int, abs_byte, abs_byte, abs_byte]};
		32 ->
			{32, entity_look, [int, byte, byte]};
		33 ->
			{33, entity_look_move, [int, abs_byte, abs_byte, abs_byte, byte, byte]};
		34 ->
			{34, entity_teleport, [int, abs_int, abs_int, abs_int, byte, byte]};
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
		51 ->
			{51, map_chunk, [int, int, chunk_data]};
		52 ->
			{52, multi_block_change, [int, int, multi_block_change_data]};
		53 ->
			{53, block_change, [int, byte, int, short, byte]};
		54 ->
			{54, block_action, [int, short, int, byte, byte, short]};
		55 ->
			{55, block_break_animation, [int, int, int, int, byte]};
		56 ->
			{56, map_chunk_bulk, [chunk_bulk]};
		60 ->
			{60, explosion, [double, double, double, float, coordinate_offsets, float, float, float]};
		61 ->
			{61, sound_particle_effect, [int, int, byte, int, int, bool]};
		62 ->
			{62, named_sound_effect, [string, int, int, int, float, byte]};
		70 ->
			{70, new_invalid_state, [byte, byte]};
		71 ->
			{71, thunderbolt, [int, bool, abs_int, abs_int, abs_int]};
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
		130 ->
			{130, update_sign, [int, short, int, string, string, string, string]};
		131 ->
			{131, item_data, [short, short, {array, short, binary}]};
		132 ->
			{132, update_tile_entity, [int, short, int, byte, short]}; % TODO!
		200 ->
			{200, increment_statistic, [int, byte]};
		201 ->
			{201, player_list_item, [string, bool, short]};
		202 ->
			{202, player_abilities, [byte, byte, byte]};
		203 ->
			{203, tab_complete, [string]};
		204 ->
			{204, client_settings, [string, byte, byte, byte, bool]};
		205 ->
			{205, client_statuses, [byte]};
		250 ->
			{250, plugin_message, [string, {array, short, byte}]};
		252 ->
			{252, encryption_key_response, [{array, short, binary}, {array, short, binary}]};
		253 ->
			{253, encryption_key_request, [string, {array, short, binary}, {array, short, binary}]};
		254 ->
			{254, server_list_ping, [byte]};
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
		update_sign -> 130;
		item_data -> 131;
		update_tile_entity -> 132;
		increment_statistic -> 200;
		player_list_item -> 201;
		player_abilities -> 202;
		client_settings -> 204;
		client_statuses -> 205;
		encryption_key_response -> 252;
		encryption_key_request -> 253;
		server_list_ping -> 254;
		disconnect -> 255;
		X ->
			{error, {unknown_name, X}}
	end).

get_enchantment_by_id(Id) ->
	case Id of
		0 -> {0, protection, [helmet, chestplate, leggings, boots], 4};
		1 -> {1, fire_protection, [helmet, chestplate, leggings, boots], 4};
		2 -> {2, feather_falling, [boots], 4};
		3 -> {3, blast_protection, [helmet, chestplate, leggings, boots], 4};
		4 -> {4, projectile_protection, [helmet, chestplate, leggings, boots], 4};
		5 -> {5, respiration, [helmet], 3};
		6 -> {6, aqua_affinity, [helmet], 1};
		
		16 -> {16, sharpness, [sword], 5};
		17 -> {17, smite, [sword], 5};
		18 -> {18, bane_of_anthropods, [sword], 5};
		19 -> {19, knockback, [sword], 2};
		20 -> {20, fire_aspect, [sword], 2};
		21 -> {21, looting, [sword], 3};
		
		48 -> {48, 'power', [bow], 5};
		49 -> {49, punch, [bow], 2};
		50 -> {50, flame, [bow], 1};
		51 -> {51, 'infinity', [bow], 1};
		
		32 -> {32, efficiency, [pickaxe, shovel, axe], 5};
		33 -> {33, silk_touch, [pickaxe, shovel, axe], 1};
		34 -> {34, unbreaking, [pickaxe, shovel, axe], 3};
		35 -> {35, fortune, [pickaxe, shovel, axe], 3}
	end.

get_enchantment_by_name(Name) ->
	get_enchantment_by_id(case Name of
		protection -> 0;
		fire_protection -> 1;
		feather_falling -> 2;
		blast_protection -> 3;
		projectile_protection -> 4;
		respiration -> 5;
		aqua_affinity -> 6;
		
		sharpness -> 16;
		smite -> 17;
		bane_of_anthropods -> 18;
		knockback -> 19;
		fire_aspect -> 20;
		looting -> 21;
		
		'power' -> 48;
		punch -> 49;
		flame -> 50;
		'infinity' -> 51;
		
		efficiency -> 32;
		silk_touch -> 33;
		unbreaking -> 34;
		fortune -> 35
	end).
		
		
