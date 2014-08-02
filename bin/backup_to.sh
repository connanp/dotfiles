#!/bin/env zsh

local -a PATTERNS
local TS=$(date +%d-%m-%Y)
local DESTDIR=${2:-/tmp}
local DEST=$DEST/home-backup-$TS.tgz

[[ -f $HOME/.backup_patterns ]] || echo "Missing $HOME/.backup_patterns file" && exit 1
while read line;
do
    PATTERNS+=($line)
done < $HOME/.backup_patterns

tar -C $HOME -cfJ $DEST $PATTERNS && rsync -avh $DEST $1 && rm -f $DEST
