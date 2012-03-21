-module(mc_erl_chat).

-export([broadcast/1, broadcast/2, to_player/2]).

-include("records.hrl").

broadcast(Player, Message) ->
	broadcast(Player#player.name ++ ": " ++ Message).

broadcast(Message) ->
	Players = mc_erl_entity_manager:get_all_players(),
	lists:map(fun(Player) -> Player#player.player_logic ! {chat, Message} end, Players).


%% Receiver = player record or name
to_player(Name, Message) when is_list(Name) ->
	Receiver = mc_erl_entity_manager:get_player(Name),
	to_player(Receiver, Message);
to_player(Receiver, Message) ->
	Receiver#player.player_logic ! {chat, Message}.
