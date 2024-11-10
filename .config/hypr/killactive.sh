#!/bin/sh
hyprctl activewindow|grep "class.*emacs" || hyprctl dispatch killactive
