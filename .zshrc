# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

LANG="en_US.UTF-8"
LANGUAGE="en_US:"
LC_ALL="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
platform=$(uname)
export TERM="xterm-256color"

# pre-load before any of this for certain situations
if [ -d $HOME/.config/site/preload ]; then
  for f in $HOME/.config/site/preload/*(.); do
    source $f
  done
fi

if [[ -z ${INSIDE_EMACS+x} ]]; then
    zstyle ':prezto:module:editor' key-bindings 'vi'
else
    zstyle ':prezto:module:editor' key-bindings 'emacs'
fi

zstyle ':prezto:module:prompt' theme 'pure'
zstyle ':prezto:module:editor' dot-expansion 'yes'
zstyle ':prezto:module:history-substring-search' color 'yes'

zstyle ':prezto:module:ssh:load' identities 'id_rsa' 'personal_rsa'

if [[ -z ${INSIDE_EMACS+x} ]]; then
    zstyle ':prezto:module:terminal' auto-title 'yes'
    zstyle ':prezto:module:terminal:window-title' format '%n@%m: %s'
    zstyle ':prezto:module:terminal:tab-title' format '%m: %s'
else
    zstyle ':prezto:module:terminal' auto-title 'no'
fi

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

setopt NUMERIC_GLOB_SORT

# must be the last thing executed, otherwise OS X fails to load the session
pmodload ssh

if [[ -z ${INSIDE_EMACS+x} ]]; then
    # extra vi mode keys
    bindkey '^P' up-history
    bindkey '^N' down-history
    bindkey '^?' backward-delete-char
    bindkey '^h' backward-delete-char
    bindkey '^w' backward-kill-word
    bindkey '^r' history-incremental-search-backward
    # save line to recall later after executing something else, like changing directory.
    bindkey '^B' push-line-or-edit
    bindkey -M vicmd "q" push-line-or-edit
    # Search based on what you typed in already
    bindkey -M vicmd "//" history-beginning-search-backward
    bindkey -M vicmd "??" history-beginning-search-forward
    export KEYTIMEOUT=1
    # http://www.zsh.org/mla/users/2009/msg00813.html
    # vi insert mode to respect backspace
    zle -A .backward-kill-word vi-backward-kill-word
    zle -A .backward-delete-char vi-backward-delete-char
fi

if [ -d $HOME/.config/site ]; then
  for f in $HOME/.config/site/*(.); do
    source $f
  done
fi
