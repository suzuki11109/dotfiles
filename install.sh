#!/bin/bash

echo ".cfg" >> .gitignore
git clone --bare git@github.com:suzuki11109/dotfiles.git $HOME/.cfg
git --git-dir=$HOME/.cfg/.git --work-tree=$HOME config --local status.showUntrackedFiles no
git --git-dir=$HOME/.cfg/.git --work-tree=$HOME checkout
git --git-dir=$HOME/.cfg/.git config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
git --git-dir=$HOME/.cfg/.git fetch origin
