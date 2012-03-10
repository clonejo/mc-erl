{application, mc_erl_app,
	[
		{mod, {mc_erl_app, []}},
		{description, "minecraft proxy"},
		{vsn, "1"},
		{modules, [mc_erl_app, mc_erl_proxy, mc_erl_packets, mc_erl_protocol]},
		{registered, []},
		{applications, [kernel, stdlib]}
	]
}.
