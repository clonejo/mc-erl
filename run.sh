#!/bin/sh
erl -pa ./ebin ./deps/*/ebin -boot start_sasl -s mc_erl_app os_run
