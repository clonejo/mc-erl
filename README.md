## Prerequisites
You'll need an Erlang compiler/vm! If you are on Linux, you can install Erlang using your distribution's package manager. E.g. on Debian/Ubuntu/Linux Mint this will do:

    sudo apt-get install erlang

You will also need make and rebar, the commonly used Erlang build tool.

## Setup

As we use an Mnesia database for storing data permanently, it has to be set up first. Always keep in mind that the table definitions can change on updates, introducing incompabilities.

1. Download (uncompress if necessary) (or use _git clone_)
2. $ make
3. $ ./setup.sh

### Starting:
    $ ./run.sh

### Stopping:
Press Ctrl+C twice.

The server runs at port 25565 (default). The port and other options can be set in "server.conf", restart the server for changes to take effect.

If you want to talk to me (clonejo), I'm idling in #mcdevs on Freenode.

## Feature list
* compatible to Minecraft 1.4.6
* see other players
* see block changes by other players
* protocol encryption

### Todo
* proper dropped item handling
* configuration system (see erlconf)
* plugin system
* see player sneaking
* have multiple entity movement routing processes in worlds with high load
* update protocol to 1.7.4
   * move protocol implementation out as an external library
