## Prerequisites
You'll need an erlang compiler/vm! When you are on Linux, you can install Erlang using your distribution's package manager. E.g. on Debian/Ubuntu/Linux Mint this will do:

    sudo apt-get install erlang

## Overall setup

1. Download (uncompress if necessary) (or use _git clone_)
2. $ ./rebar compile

## Server
As we now use Mnesia for storing data permanently, it has to be set up first. Always keep in mind that the table definitions can change on updates, introducing incompabilities. Use the .bat files on Windows.

    $ ./setup.sh

### Starting:
    $ ./run.sh

### Stopping:
Press Ctrl+C

The server runs at port 25565 (default). The port and other options can be set in "server.conf", restart the server for changes taking effect.

## PS:
The Erlang shell can be left with "q().", which shuts down the server.

## Feature list
* compatible to Minecraft 1.4.5
* see other players
* see block changes by other players
* protocol encryption

### Todo
* proper item handling
* configuration system (see erlconf)
* plugin system
* see player sneaking
* have multiple entity movement routing processes in worlds with high load
