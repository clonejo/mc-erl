-module(mc_erl_player_core).
-export([init_player/1]).

init_player(Socket) ->
	proc_lib:init_ack(self()),
	read(Socket).

read(Socket) ->
	{ok, Packet} = mc_erl_protocol:decode_packet(Socket),
	case Packet of
		{server_list_ping, [] } ->
			io:format("[~s] Nope, just wanted to ping~n", [?MODULE]),
			send(Socket, {disconnect,["A Minecraft Server§0§20"]});
			%gen_tcp:close(Socket);
		{handshake, [S]} ->
			{Name,_} = lists:split(string:str(S,";")-1,S),
			io:format("[~s] Player joining: ~s~n", [?MODULE, Name]),
			send(Socket, {handshake, ["-"]}),
			
			% last packet to read sequentially and then we're going full asynchronous mode
			{ok, {login_request, [28, Name, "", 0, 0, 0, 0, 0]}}
				= mc_erl_protocol:decode_packet(Socket),
				
			Writer = spawn_link(fun() -> async_writer(Socket) end),
			Logic = spawn_link(fun() -> mc_erl_player_logic:init_logic(Writer, Name) end),
			Logic ! login_sequence,
			
			spawn_link(fun() -> keep_alive_sender(Socket) end),
			read(Socket, Logic)
	end.

read(Socket, Logic) ->
	case mc_erl_protocol:decode_packet(Socket) of
		{error,closed} ->
			% remove players' entity id etc.
			io:format("[~s] Lost connection~n", [?MODULE]);

		{ok, Packet} ->
			Logic ! {packet, Packet},
			read(Socket, Logic)
	end.

% dummy sender, doesn't check for reply
keep_alive_sender(Socket) ->
	send(Socket, {keep_alive,  [0]}),
	receive after 1000 -> ok end,
	keep_alive_sender(Socket).

% asynchronous writer to pass on to logic
async_writer(Socket) ->
	receive
		stop -> ok;
		{packet, Data} -> 
			send(Socket, Data),
			async_writer(Socket)
	end.

send(Socket, Packet) ->
	gen_tcp:send(Socket, mc_erl_protocol:encode_packet(Packet)).
