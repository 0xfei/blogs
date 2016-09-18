#!/bin/bash

echo "Push public/* to 0x01f.com !"

echo "Start pushing..."

filename="1.tar.gz"
origfiles="public/*"
remote="root@0x01f.com:/var/www/"
sshdir="/var/www/"
cmd="cd ${sshdir}; rm -rf public; tar -xzf $filename -C $sshdir; rm $filename"

tar -czf $filename $origfiles
scp $filename $remote
ssh root@0x01f.com $cmd

echo "Remove $filename."
rm $filename

echo "Done!"

