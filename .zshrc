LANG="en_US.UTF-8"
LANGUAGE="en_US:"
LC_ALL="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
platform=$(uname)
export TERM="xterm-256color"
export EDITOR="vim"

# pre-load before any of this for certain situations
if [ -d $HOME/.config/site/preload ]; then
  for f in $HOME/.config/site/preload/*; do
    source $f
  done
fi

PATH="/usr/local/bin:${PATH}:$HOME/bin"
typeset -U PATH  # remove dupes
export PATH

zstyle ':prezto:module:prompt' theme 'connanp'

zstyle ':prezto:module:editor' key-bindings 'vi'

zstyle ':prezto:module:history-substring-search' color 'yes'

zstyle ':prezto:module:ssh:load' identities 'id_rsa' 'personal_rsa'

zstyle ':prezto:module:terminal' auto-title 'yes'
zstyle ':prezto:module:terminal:window-title' format '%n@%m: %s'
zstyle ':prezto:module:terminal:tab-title' format '%m: %s'

zstyle ':prezto:module:syntax-highlighting' highlighters \
  'main' \
  'brackets' \
  'pattern' \
  'cursor' \
  'root'

zstyle ':prezto:load' pmodule \
  'environment' \
  'terminal' \
  'editor' \
  'history' \
  'directory' \
  'spectrum' \
  'utility' \
  'completion' \
  'archive' \
  'osx' \
  'git' \
  'fasd' \
  'python' \
  'rsync' \
  'syntax-highlighting' \
  'history-substring-search' \
  'prompt'

source $HOME/.zprezto/runcoms/zshrc
# vi mode text-objects, e.g.: ciw
source $HOME/.config/opp.zsh/opp.zsh
source $HOME/.config/opp.zsh/opp/*
source $HOME/.aliases

# extra vi mode keys
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
export KEYTIMEOUT=1
# http://www.zsh.org/mla/users/2009/msg00813.html
# vi insert mode to respect backspace
zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char

export WORKON_HOME=$HOME/.virtualenvs
[ -f /usr/local/bin/virtualenvwrapper.sh ] && source /usr/local/bin/virtualenvwrapper.sh

if [[ "$platform" == "Darwin" ]]; then
  source $HOME/.ssh/environment-$(hostname -s)
fi

if [ -d $HOME/.config/site ]; then
  for f in $HOME/.config/site/*; do
    source $f
  done
fi

# must be the last thing executed, otherwise OS X fails to load the session
pmodload ssh
