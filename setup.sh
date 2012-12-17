#!/bin/sh

echo "generating rsa keypair..."
ssh-keygen -b 1024 -t rsa -f key

echo "setting up database..."
erl -pa ./ebin -s mc_erl_app os_setup

echo "done!"