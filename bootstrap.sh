#!/bin/bash

link () {
  for file in $(ls -A | grep -vE 'bootstrap.sh$|\.git$|init.vim$'); do
    ln -sv "$PWD/$file" "$HOME"
  done

  ln -sv init.vim "$HOME/.config/nvim/init.vim"

  echo "Symlinkin completed"
}

link
