#!/bin/env bash

killall -9 pipewire pipewire-pulse wireplumber

pipewire &
# should be auto-launched from dbus/configs
# pipewire-pulse &
# wireplumber &
