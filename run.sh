#!/bin/sh

echo "generating rsa keypair..."
rm -f key key.pub
ssh-keygen -b 1024 -t rsa -f key -q -P ""

erl -pa ./ebin -boot start_sasl -s mc_erl_app os_run
