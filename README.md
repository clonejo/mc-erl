## Prerequisites
You'll need an erlang compiler/vm! When you are on Linux, you can install Erlang using your distribution's package manager. E.g. on Debian/Ubuntu/Linux Mint this will do:

    sudo apt-get install erlang

## Overall setup

1. Download (uncompress if necessary) (or use _git clone_)
2. $ python metamake.py
3. $ sh make.sh
4. $ mkdir log
5. $ cd ebin/
6. $ erl

## Proxy
There are two different types of operation. It is either a fully functional minecraft proxy server or an alpha minecraft 1.2 SMP server. To run it as a proxy server, type

    application:start(mc_erl_proxy).

into Erlang prompt. How to use the server is described below.

The proxy will be available at 127.0.0.1:25566 and it proxies to 127.0.0.1:25565.
Be aware the log files can get quite big (some 100MB).

## Server
As we now use Mnesia for storing data permanently, it has to be set up first. Always keep in mind that the table definitions can change on updates, introducing incompabilities.

Launch an Erlang shell in ebin/ (after compiling; be aware the dot at each line's end is necessary):

1. mnesia:create_schema([node()]).
2. mnesia:start().
3. mc_erl_server_sup:setup().

### Starting:
1. mnesia:start(). % if necessary 
2. application:start(mc\_erl\_server).

### Stopping:
1. application:stop(mc\_erl\_server).
2. mnesia:stop().


The server runs at port 25565 (default). The port and other options can be set in "server.conf", restart the server for changes taking effect.

## PS:
The Erlang shell can be left with "q().".

