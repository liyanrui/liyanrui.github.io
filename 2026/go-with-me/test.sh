#!/bin/bash
exec > /tmp/loop.log 2>&1
for ((i = 1; i < 30; i++)); do
    echo "iter $i"
    echo -n "127.0.0.1:8080/" > /tmp/hi-pipe
    echo "status: $?"
done
echo "loop finished"
