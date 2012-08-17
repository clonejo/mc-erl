-module(mc_erl_player_core).
-export([init_player/1]).

init_player(Socket) ->
	proc_lib:init_ack(self()),
	read(Socket).

read(Socket) ->
	{ok, Packet} = mc_erl_protocol:decode_packet(Socket),
	case Packet of
		{server_list_ping, [] } ->
			%io:format("[~s] Nope, just wanted to ping~n", [?MODULE]),
			send(Socket, {disconnect,[lists:flatten([
				mc_erl_config:get(description, []),167,
				integer_to_list(mc_erl_entity_manager:player_count()),167,"100"])]}),
			gen_tcp:close(Socket);

		{handshake, [39, Name, _Host, _Port]} ->
			io:format("[~s] Player joining: ~s~n", [?MODULE, Name]),
			
			Writer = proc_lib:spawn_link(fun() -> async_writer(Socket) end),
			
			Logic = mc_erl_player_logic:start_logic(Writer, Name),
			mc_erl_player_logic:packet(Logic, login_sequence),
			
			proc_lib:spawn_link(fun() -> process_flag(trap_exit, true), keep_alive_sender(Socket) end),
			read(Socket, Logic)
	end.

read(Socket, Logic) when is_pid(Logic) ->
	case mc_erl_protocol:decode_packet(Socket) of
		{error,closed} ->
			%io:format("[~s] socket is closed~n", [?MODULE]),
			mc_erl_player_logic:packet(Logic, net_disconnect);

		{ok, Packet} ->
			mc_erl_player_logic:packet(Logic, {packet, Packet}),
			read(Socket, Logic)
	end.

% dummy sender, doesn't check for reply
keep_alive_sender(Socket) ->
	send(Socket, {keep_alive,  [0]}),
	receive
		{'EXIT', _, _} -> ok
		after 1000 ->
			keep_alive_sender(Socket)
	end.

% asynchronous writer to pass on to logic
async_writer(Socket) ->
	receive
		stop -> 
			gen_tcp:close(Socket),
			ok;
		{packet, Data} ->
			send(Socket, Data),
			async_writer(Socket)
	end.

send(Socket, Packet) ->
	gen_tcp:send(Socket, mc_erl_protocol:encode_packet(Packet)).
