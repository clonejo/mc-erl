-module(mc_erl_app).
-export([setup/0, start/2, stop/1]).

%% initial server setup, needs to be done only once
setup() ->
	mc_erl_chunk_manager:setup().

start(_StartType, _StartArgs) -> mc_erl_server_sup:start_link();

stop(_State) -> mc_erl_server_sup:shutdown().
