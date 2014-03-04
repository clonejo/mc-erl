%% @copyright 2012-2013 Gregory Fefelov, Feiko Nanninga

-module(mc_erl_server).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

-include("records.hrl").

-record(state, {listen, public_key, private_key}).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").


% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).


% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    lager:info("[~s] starting~n", [?MODULE]),
    %PublicKey = read_public_key("key.pub"),
    %PrivateKey = read_private_key("key"),

    {ok, PrivateKey} = cutkey:rsa(1024, 65537, [{return, key}]),
    #'RSAPrivateKey'{modulus=Modulus, publicExponent=PublicExponent} = PrivateKey,
    {'SubjectPublicKeyInfo', PublicKey, not_encrypted} = public_key:pem_entry_encode('SubjectPublicKeyInfo', #'RSAPublicKey'{modulus=Modulus, publicExponent=PublicExponent}),

    Port = mc_erl_config:get(port, 25565),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {reuseaddr, true}, {active, false},
                                         {packet, raw}, {nodelay, true}]),
    spawn_link(fun() -> acceptor(Listen) end),
    spawn_link(fun() -> ticker() end),
    {ok, #state{listen=Listen, public_key=PublicKey, private_key=PrivateKey}}.

acceptor(Listen) ->
    lager:debug("[~s:acceptor] awaiting connection...~n", [?MODULE]),
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            gen_server:cast(?MODULE, {new_connection, Socket}),
            acceptor(Listen);
        {error, closed} ->
            ok
    end.

ticker() -> ticker(0).
ticker(Time) ->
    gen_server:cast(?MODULE, {tick, Time}),
    timer:sleep(50),
    ticker(Time+1).

handle_call(Message, _From, State) ->
    lager:notice("[~s] received call: ~p~n", [?MODULE, Message]),
    {noreply, State}.

handle_cast({new_connection, Socket}, State) ->
    Pid = proc_lib:start(mc_erl_player_core, init_player, [Socket, State#state.public_key, State#state.private_key]),
    gen_tcp:controlling_process(Socket, Pid),
    {noreply, State};

handle_cast({tick, Time}=Tick, State) when is_integer(Time) ->
    mc_erl_entity_manager:broadcast(Tick),
    mc_erl_chunk_manager:tick(Time),
    {noreply, State};

handle_cast(stop, State) ->
    lager:info("[~s] stopping~n", [?MODULE]),
    {stop, normal, State};

handle_cast(Message, State) ->
    lager:notice("[~s] received cast: ~p~n", [?MODULE, Message]),
    {noreply, State}.

handle_info(Message, State) ->
    lager:notice("[~s] received info: ~p~n", [?MODULE, Message]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:notice("[~s] terminated with Reason=~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

