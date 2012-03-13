-module(mc_erl_app).
-export([start/2, stop/1]).


%% StartArgs = proxy | server
%% StartArgs = [] is deprecated!
start(_StartType, StartArgs) ->
	case StartArgs of
		[] -> mc_erl_proxy:start_link();
		[proxy] -> mc_erl_proxy:start_link();
		[server] -> mc_erl_server:start_link();
stop(_State) -> mc_erl_proxy:stop().
