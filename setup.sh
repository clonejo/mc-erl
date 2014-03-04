#!/bin/sh

echo "setting up database..."
erl -pa ./ebin -config apps.config -s mc_erl_app os_setup

echo "done!"
