#!/bin/bash

if [ $(swaync-client -D) = "false" ]; then
	pw-cat -p --volume 0.3 /home/connan/.config/swaync/notification.mp3 2>/dev/null
fi
