#!/bin/sh

echo "Start buildDrafts!"
hugo -s ./
sudo cp -R public/* /var/www/
