#!/usr/bin/env zsh


setopt EXTENDED_GLOB
for rcfile in "${HOME}"/repos/dotfiles/.zprezto/runcoms/^README.md(.N); do
    ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
