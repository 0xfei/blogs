#!/bin/bash

echo "Push public/* to 0x01f.cn !"

echo "Start pushing..."

filename="1.tar.gz"
origfiles="public/*"
remote="root@0x01f.cn:/var/www/"
sshdir="/var/www/"
# Do not execute `rm -rf public`
cmd="cd ${sshdir}; tar -xzf $filename -C $sshdir; rm $filename"

tar -czf $filename $origfiles
scp $filename $remote
ssh root@0x01f.cn $cmd
echo "Remove $filename."
rm $filename

echo "Done!"

