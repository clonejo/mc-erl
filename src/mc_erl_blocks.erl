%% @copyright 2012 Gregory Fefelov, Feiko Nanninga

-module(mc_erl_blocks).

-export([block_info/1, can_build/1]).

-include("records.hrl").

%% currently only lists blocks placeable in creative
block_info(Id) ->
	case Id of
		  0 -> #block_type{id=  0, name="air", placeable=true};
		  1 -> #block_type{id=  1, name="stone", placeable=true};
		  2 -> #block_type{id=  2, name="grass", placeable=true};
		  3 -> #block_type{id=  3, name="dirt", placeable=true};
		  4 -> #block_type{id=  4, name="cobblestone", placeable=true};
		  5 -> #block_type{id=  5, name="wooden planks", placeable=true};
		  6 -> #block_type{id=  6, name="saplings", placeable=true};
		  7 -> #block_type{id=  7, name="bedrock", placeable=true};
		 12 -> #block_type{id= 12, name="sand", placeable=true};
		 13 -> #block_type{id= 13, name="gravel", placeable=true};
		 14 -> #block_type{id= 14, name="gold ore", placeable=true};
		 15 -> #block_type{id= 15, name="iron ore", placeable=true};
		 16 -> #block_type{id= 16, name="coal ore", placeable=true};
		 17 -> #block_type{id= 17, name="wood", placeable=true};
		 18 -> #block_type{id= 18, name="leaves", placeable=true};
		 19 -> #block_type{id= 19, name="sponge", placeable=true};
		 20 -> #block_type{id= 20, name="glass", placeable=true};
		 21 -> #block_type{id= 21, name="lapis lazuli ore", placeable=true};
		 22 -> #block_type{id= 22, name="lapis lazuli block", placeable=true};
		 23 -> #block_type{id= 23, name="dispenser", placeable=true};
		 24 -> #block_type{id= 24, name="sandstone", placeable=true};
		 25 -> #block_type{id= 25, name="note block", placeable=true};
		 27 -> #block_type{id= 27, name="powered rail", placeable=true};
		 28 -> #block_type{id= 28, name="detector rail", placeable=true};
		 29 -> #block_type{id= 29, name="sticky piston", placeable=true};
		 30 -> #block_type{id= 30, name="cobweb", placeable=true};
		 31 -> #block_type{id= 31, name="tall grass", placeable=true};
		 32 -> #block_type{id= 32, name="dead bush", placeable=true};
		 33 -> #block_type{id= 33, name="piston", placeable=true};
		 35 -> #block_type{id= 35, name="wool", placeable=true};
		 37 -> #block_type{id= 37, name="dandelion", placeable=true};
		 38 -> #block_type{id= 38, name="rose", placeable=true};
		 39 -> #block_type{id= 39, name="brown mushroom", placeable=true};
		 40 -> #block_type{id= 40, name="red mushroom", placeable=true};
		 41 -> #block_type{id= 41, name="gold block", placeable=true};
		 42 -> #block_type{id= 42, name="iron block", placeable=true};
		 44 -> #block_type{id= 44, name="slabs", placeable=true};
		 45 -> #block_type{id= 45, name="bricks", placeable=true};
		 46 -> #block_type{id= 46, name="tnt", placeable=true};
		 47 -> #block_type{id= 47, name="bookshelf", placeable=true};
		 48 -> #block_type{id= 48, name="moss stone", placeable=true};
		 49 -> #block_type{id= 49, name="obsidian", placeable=true};
		 50 -> #block_type{id= 50, name="torch", placeable=true};
		 53 -> #block_type{id= 53, name="wooden stairs", placeable=true};
		 54 -> #block_type{id= 54, name="chest", placeable=true};
		 56 -> #block_type{id= 56, name="diamond ore", placeable=true};
		 57 -> #block_type{id= 57, name="diamond block", placeable=true};
		 58 -> #block_type{id= 58, name="crafting table", placeable=true};
		 61 -> #block_type{id= 61, name="furnace", placeable=true};
		 65 -> #block_type{id= 65, name="ladders", placeable=true};
		 66 -> #block_type{id= 66, name="rails", placeable=true};
		 67 -> #block_type{id= 67, name="cobblestone stairs", placeable=true};
		 69 -> #block_type{id= 69, name="lever", placeable=true};
		 70 -> #block_type{id= 70, name="stone pressure plate", placeable=true};
		 72 -> #block_type{id= 72, name="wooden pressure plate", placeable=true};
		 73 -> #block_type{id= 73, name="redstone ore", placeable=true};
		 76 -> #block_type{id= 76, name="redstone torch", placeable=true};
		 77 -> #block_type{id= 77, name="stone button", placeable=true};
		 79 -> #block_type{id= 79, name="ice", placeable=true};
		 80 -> #block_type{id= 80, name="snow block", placeable=true};
		 81 -> #block_type{id= 81, name="cactus", placeable=true};
		 82 -> #block_type{id= 82, name="clay block", placeable=true};
		 84 -> #block_type{id= 84, name="jukebox", placeable=true};
		 85 -> #block_type{id= 85, name="fence", placeable=true};
		 86 -> #block_type{id= 86, name="pumpkin", placeable=true};
		 87 -> #block_type{id= 87, name="netherrack", placeable=true};
		 88 -> #block_type{id= 88, name="soul sand", placeable=true};
		 89 -> #block_type{id= 89, name="glowstoneg block", placeable=true};
		 91 -> #block_type{id= 91, name="jack-o-lantern", placeable=true};
		 96 -> #block_type{id= 96, name="trapdoor", placeable=true};
		 98 -> #block_type{id= 98, name="stone bricks", placeable=true};
		101 -> #block_type{id=101, name="iron bars", placeable=true};
		102 -> #block_type{id=102, name="glass pane", placeable=true};
		103 -> #block_type{id=103, name="melon", placeable=true};
		106 -> #block_type{id=106, name="vines", placeable=true};
		107 -> #block_type{id=107, name="fence gate", placeable=true};
		108 -> #block_type{id=108, name="brick stairs", placeable=true};
		109 -> #block_type{id=109, name="stone brick stairs", placeable=true};
		110 -> #block_type{id=110, name="mycelium", placeable=true};
		111 -> #block_type{id=111, name="lily pad", placeable=true};
		112 -> #block_type{id=112, name="nether brick", placeable=true};
		113 -> #block_type{id=113, name="nether brick fence", placeable=true};
		114 -> #block_type{id=114, name="nether brick stairs", placeable=true};
		116 -> #block_type{id=116, name="enchantment table", placeable=true};
		121 -> #block_type{id=121, name="end stone", placeable=true};
		122 -> #block_type{id=122, name="dragon egg", placeable=true};
		123 -> #block_type{id=123, name="redstone lamp", placeable=true};
		_   -> #block_type{}
	end.

can_build(Id) ->
	(block_info(Id))#block_type.placeable.
