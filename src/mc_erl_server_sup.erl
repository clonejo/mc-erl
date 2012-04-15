-module(mc_erl_server_sup).
-behaviour(supervisor).

-export([setup/0, start_link/0, shutdown/0, init/1]).

%% initial server setup, needs to be done only once
setup() ->
	mc_erl_chunk_manager:setup().

init(_Args) ->
	{ok, {{one_for_one, 3, 15}, 
		[{mc_erl_config, {mc_erl_config, start_link, []},
			permanent, 2000, worker, [mc_erl_config]},
		 {mc_erl_chunk_manager, {mc_erl_chunk_manager, start_link, []},
		 	permanent, 2000, worker, [mc_erl_chunk_manager]},
		 {mc_erl_entity_manager, {mc_erl_entity_manager, start_link, []},
			permanent, 2000, worker, [mc_erl_entity_manager]},
		 {mc_erl_server, {mc_erl_server, start_link, []},
			permanent, 2000, worker, [mc_erl_server]}
		 ]}
	}.

start_link() -> supervisor:start_link(mc_erl_server_sup, []).
shutdown() ->
     exit(whereis(?MODULE), shutdown).
