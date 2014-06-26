LANG="en_US.UTF-8"
LANGUAGE="en_US:"
LC_ALL="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
platform=$(uname)
export TERM="xterm-256color"
ZSH_CUSTOM=$HOME/.config/oh-my-zsh
########## Begin environment variables for /opt/ssh-wrapper {{{

#SSH_AUTH_SOCK="${HOME}/.ssh/environment-$(hostname -s)"
#export SSH_AUTH_SOCK

########## }}} End of environment variables for /opt/ssh-wrapper

########## Begin path additions for /opt/ssh-wrapper {{{

#PATH="/opt/ssh-wrapper/bin:${PATH}:${HOME}/Cloud9BrazilBuild-1.0/bin"
PATH="/usr/local/bin:${PATH}:${HOME}/Cloud9BrazilBuild-1.0/bin:${HOME}/Library/Python/2.7/bin"
typeset -U PATH  # remove dupes
export PATH

########## }}} End of path additions for /opt/ssh-wrapper
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="connanp"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git jira extract osx virtualenv virtualenvwrapper vi-mode)

source $ZSH/oh-my-zsh.sh
source ~/.aliases

# vi mode
bindkey -v
bindkey '^R' history-incremental-search-backward

export WORKON_HOME=$HOME/.virtualenvs
[ -f /usr/local/bin/virtualenvwrapper.sh ] && source /usr/local/bin/virtualenvwrapper.sh

JIRA_URL="https://network-tracking.amazon.com"

if [[ "$platform" == "Darwin" ]]; then
  source $HOME/.ssh/environment-$(hostname -s)
fi
