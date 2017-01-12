#!/bin/sh

echo "Start buildDrafts!"
hugo -s ./
./auto_push.sh
