-module(mc_erl_app).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) -> mc_erl_server_sup:start_link();

stop(_State) -> mc_erl_server_sup:shutdown().
