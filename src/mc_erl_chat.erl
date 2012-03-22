-module(mc_erl_chat).

-export([broadcast/1, broadcast/2, to_player/2]).

-include("records.hrl").

broadcast(Player, Message) ->
	broadcast(Player#player.name ++ ": " ++ Message).

broadcast(Message) ->
	Players = mc_erl_entity_manager:get_all_players(),
	lists:map(fun(Player) -> to_player(Player, Message) end, Players).


%% Receiver = player record or name or pid
to_player(Name, Message) when is_list(Name) ->
	Receiver = mc_erl_entity_manager:get_player(Name),
	to_player(Receiver, Message);
	
to_player(Receiver, Message) when is_tuple(Receiver) ->
	to_player(Receiver#player.player_logic, Message);
	
to_player(Logic, Message) when is_pid(Logic) ->
	Parts = split_message(Message),
	lists:map(fun(Part) -> mc_erl_player_logic:packet({chat, Part}} end, Parts).


split_message(Message) ->
	split_message(Message, []).

split_message(Message, Output) ->
	if length(Message) =< 119 ->
		lists:reverse([Message|Output]);
	true ->
		{Part, Rest} = lists:split(119, Message),
		split_message(Rest, [Part|Output])
	end.


