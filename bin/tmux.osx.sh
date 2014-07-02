#!/usr/bin/env bash
##################
# TMUX auto-start

_TMUX="tmux"
# abort if we're already inside a TMUX session
[ "$TMUX" = "" ] || exit 0

# startup a "default" session if none currently exists
$_TMUX has-session -t _default 2>/dev/null || $_TMUX new-session -s _default -d

# present menu for user to choose which workspace to open
PS3="Please choose your session: "
options=($($_TMUX list-sessions -F "#S") "NEW SESSION" "ZSH")
echo "Available sessions"
echo "------------------"
echo " "
select opt in "${options[@]}"
do
  case $opt in
    "NEW SESSION")
      read -p "Enter new session name: " SESSION_NAME
      $_TMUX -2 -CC new -s "$SESSION_NAME"
      break
      ;;
    "ZSH")
      zsh --login
      break;;
    *)
      $_TMUX -2 -CC attach-session -t $opt
      break
      ;;
  esac
done
