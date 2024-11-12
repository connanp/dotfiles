PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>$HOME/tmp/startlog.$$
    setopt xtrace prompt_subst
fi

fpath=(~/.local/zsh/completion $fpath)

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

LANG="en_US.UTF-8"
LANGUAGE="en_US:"
LC_ALL="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
platform=$(uname)

# pre-load before any of this for certain situations
if [ -d $HOME/.config/site/preload ]; then
  for f in $HOME/.config/site/preload/*(.); do
    source $f
  done
fi

# Emacs and other terms
if [[ "$TERM" == "dumb" || "$TERM_PROGRAM" == "WarpTerminal" ]]; then
    zstyle ':prezto:module:prompt' theme 'minimal'
    zstyle ':prezto:module:terminal' auto-title 'no'
    zstyle ':prezto:module:editor' key-bindings 'emacs'
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    # unfunction precmd
    # unfunction preexec
    PS1='$ '
    export PS1
else
    zstyle ':prezto:module:prompt' theme 'minimal'
    zstyle ':prezto:module:prompt' managed 'yes'
    zstyle ':prezto:module:terminal' auto-title 'yes'
    zstyle ':prezto:module:terminal:window-title' format '%n@%m: %s'
    zstyle ':prezto:module:terminal:tab-title' format '%m: %s'
    zstyle ':prezto:module:editor' key-bindings 'vi'

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
    # Search backwards and forwards with a pattern
    bindkey -M vicmd '/' history-incremental-pattern-search-backward
    bindkey -M vicmd '?' history-incremental-pattern-search-forward

    # set up for insert mode too
    bindkey -M viins '^R' history-incremental-pattern-search-backward
    bindkey -M viins '^F' history-incremental-pattern-search-forward
    export KEYTIMEOUT=1
    # http://www.zsh.org/mla/users/2009/msg00813.html
    # vi insert mode to respect backspace
    zle -A .backward-kill-word vi-backward-kill-word
    zle -A .backward-delete-char vi-backward-delete-char
zstyle ':completion:*' squeeze-slashes true
zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(|.|..) ]] && reply=(..)'

zstyle ':prezto:module:editor' dot-expansion 'no'
zstyle ':prezto:module:history-substring-search' color 'yes'
zstyle ':prezto:module:autosuggestions' color 'yes'


zstyle ':prezto:module:syntax-highlighting' highlighters \
  'main' \
  'brackets' \
  'pattern' \
  'cursor' \
  'root'
zstyle ':prezto:module:history-substring-search' color 'no'

zstyle ':prezto:module:python' skip-virtualenvwrapper-init 'on'
zstyle ':prezto:module:python:virtualenv' initialize 'no'

# needs external source for the .zsh files i guess
#zstyle ':prezto:module:fzf' key-bindings 'yes'
#zstyle ':prezto:module:fzf' completion 'yes'

zstyle ':prezto:load' pmodule-dirs $HOME/.zprezto-contrib
zstyle ':prezto:load' pmodule \
  'environment' \
  'terminal' \
  'editor' \
  'history' \
  'directory' \
  'utility' \
  'asdf' \
  'completion' \
  'archive' \
  'git' \
  'fasd' \
  'rsync' \
  'gpg' \
  'syntax-highlighting' \
  'history-substring-search' \
  'fzf' \
  'zsh-fzf-history-search' \
  'prompt'
fi

source $HOME/.zprezto/runcoms/zshrc
source $HOME/.aliases
autoload -Uz zmv

setopt NUMERIC_GLOB_SORT

if [[ "$TERM" != "dumb" && "$TERM_PROGRAM" != "WarpTerminal" ]]; then
    # (( ${+commands[direnv]} )) && emulate zsh -c "$(asdf exec direnv export zsh)"

    # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
    # Initialization code that may require console input (password prompts, [y/n]
    # confirmations, etc.) must go above this block, everything else may go below.
    #if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    #source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
    #fi

    export DIRENV_LOG_FORMAT=''

    # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
    #[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
    #if [[ -f ~/.bash-my-aws/aliases ]]; then
    #  export PATH="$PATH:$HOME/.bash-my-aws/bin"
    #  source ~/.bash-my-aws/aliases

    #  # For ZSH users, uncomment the following two lines:
    #  autoload -U +X compinit && compinit
    #  autoload -U +X bashcompinit && bashcompinit

    #  source ~/.bash-my-aws/bash_completion.sh
    #fi
fi

if [ -d $HOME/.config/site ]; then
 for f in $HOME/.config/site/*(.); do
   source $f
 done
fi

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte-2.91.sh
fi

if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
fi


# add Pulumi to the PATH
export PATH=$PATH:$HOME/.pulumi/bin

export SSH_AUTH_SOCK=/var/run/user/1000/keyring/ssh;

eval "$(starship init zsh)"

function z() {
    [ $# -gt 0 ] && fasd_cd -d "$*" && return
    cd "$(fasd_cd -d -l 2>&1 | fzf --height 40% --nth 2.. --reverse --inline-info +s --tac --query "${*##-* }" | sed 's/^[0-9,.]* *//')"
  }
