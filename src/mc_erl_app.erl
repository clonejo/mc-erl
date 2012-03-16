-module(mc_erl_app).
-export([start/2, stop/1]).


%% StartArgs = proxy | server
%% StartArgs = [] is deprecated!
start(_StartType, StartArgs) ->
	case StartArgs of
		[] -> mc_erl_proxy:start_link();
		[proxy] -> mc_erl_proxy:start_link();
		[server] -> [
			mc_erl_server:start_link(),
			mc_erl_chunk_manager:start_link()]
	end.
stop(_State) -> [
	mc_erl_proxy:stop(),
	mc_erl_server:stop(),
	mc_erl_chunk_manager:stop()].
