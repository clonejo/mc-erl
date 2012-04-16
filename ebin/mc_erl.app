{application, mc_erl_server,
	[
		{mod, {mc_erl_app, [server]}},
		{description, "minecraft server"},
		{vsn, "1"},
		{modules, [mc_erl_server_sup, mc_erl_server, mc_erl_packets, mc_erl_protocol,
			mc_erl_chunk_manager, mc_erl_chunk_generator, nbt]},
		{registered, []},
		{applications, [kernel, stdlib, mnesia]}
	]
}.
