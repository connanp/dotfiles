#!/bin/sh

# This allows you to control which image to init the daemon with according
# to the time of day. You may change the match cases as you see fit.
# This currently only takes hours into account, but it should be easy to
# modify to also use minutes, or days of the week, if you want.
#
# Use it simply by calling this script instead of swww-daemon

export SWWW_TRANSITION_FPS=60
export SWWW_TRANSITION_STEP=2
export SWWW_TRANSITION=wave

case $(date +%H) in
	00 | 01 | 02 | 03 | 04 | 05 | 06 | 07) # First 8 hours of the day

	        swww img ~/wallpapers/bl2.jpeg
		;;
	08 | 09 | 10 | 11 | 12 | 13 | 14 | 15) # Middle 8 hours of the day

	        swww img ~/wallpapers/anm-city.gif
		;;
	16 | 17 | 18 | 19 | 20 | 21 | 22 | 23) # Final 8 hours of the day

		swww img ~/wallpapers/bl1.jpg
		;;
esac
