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

## Server
As we now use Mnesia for storing data permanently, it has to be set up first. Always keep in mind that the table definitions can change on updates, introducing incompabilities.

Launch an Erlang shell in ebin/ (after compiling; be aware the dot at each line's end is necessary):

1. mnesia:create_schema([node()]).
2. mnesia:start().
3. mc_erl_app:setup().

### Starting:
1. mnesia:start(). % if necessary 
2. application:start(mc\_erl\_server).

### Stopping:
1. application:stop(mc\_erl\_server).
2. mnesia:stop().


The server runs at port 25565 (default). The port and other options can be set in "server.conf", restart the server for changes taking effect.

## PS:
The Erlang shell can be left with "q().".

