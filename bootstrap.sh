#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")"

git pull origin master
git submodule init
git submodule update --init --recursive
git submodule update --recursive

function doIt() {
  rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" \
    --exclude "README.md" -avh --no-perms . ~;
  ln -s $HOME/.zprezto/zlogin $HOME/.zlogin
  ln -s $HOME/.zprezto/zlogout $HOME/.zlogout
  ln -s $HOME/.zprezto/zpreztorc $HOME/.zpreztorc
  ln -s $HOME/.zprezto/zprofile $HOME/.zprofile
  ln -s $HOME/.zprezto/zshenv $HOME/.zshenv
  ln -s $HOME/.config/zprezto/prompt/* $HOME/.zprezto/modules/prompt/functions/
}

if [ "$1" = "--force" -o "$1" = "-f" ]; then
  doIt
else
  read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
  echo ""
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    doIt
  fi
fi
unset doIt;
