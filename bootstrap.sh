#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")"

git pull origin master
git submodule init
git submodule update --init --recursive
git submodule update --recursive

function doIt() {
  rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" \
    --exclude "README.md" --exclude "applescript/" -avh --no-perms . ~;
  ln -sf $HOME/.zprezto/runcoms/zlogin $HOME/.zlogin
  ln -sf $HOME/.zprezto/runcoms/zshenv $HOME/.zshenv
  ln -sf $HOME/.config/zprezto/prompt/* $HOME/.zprezto/modules/prompt/functions/
  # custom modules
  exclude="--exclude='.gitmodules' --exclude='README.md'"
  if [[ ! -f "$HOME/.gitconfig" ]]; then
    exclude="${exclude} --exclude='.git*'"
  fi
  rsync -avh --no-perms $exclude $HOME/.config/zprezto/modules $HOME/.zprezto/
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
