#!/bin/bash
echo "git add . && git commit -m \"${1}\""
git add .
Message="nothing special"
if [ $# = 1 ]; then
	Message="${1}"
fi

git commit -m "${Message}"
git push origin master

