-module(mc_erl_app).
-export([start/2, stop/1]).


%% StartArgs = proxy | server
%% StartArgs = [] is deprecated!
start(_StartType, [server]) -> mc_erl_server_sup:start_link();
start(_StartType, [proxy]) -> mc_erl_proxy:start_link().

stop(_State) -> [
	mc_erl_proxy:stop(),
	mc_erl_server_sup:shutdown()].
