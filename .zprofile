#
# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

export EDITOR='emacsclient -n'
export VISUAL='emacsclient -n'
export PAGER='less'

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

#
# Paths
#

# Set the list of directories that Zsh searches for programs.
path=(
    $HOME/bin
    /usr/local/{bin,sbin}
    $path
)

if [ -d $HOME/.config/site ]; then
  for f in $HOME/.config/site/*(.); do
    source $f
  done
fi

if (( $+commands[rbenv] )); then
    eval "$(command rbenv init - --no-rehash zsh)"
fi

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# zsh on rhel doesn't handle (( $+commands[lesspipe.sh] ))
# because of a bug with dots inside (()), it treats them as floating point numbers
if [ "$(command -v lesspipe.sh)" != "" ]; then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

#
# Temporary Files
#

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi
