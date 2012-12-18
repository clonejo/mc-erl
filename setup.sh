#!/bin/sh

echo "setting up database..."
erl -pa ./ebin -s mc_erl_app os_setup

echo "done!"