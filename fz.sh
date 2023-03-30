#!/bin/bash

export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

eval "$(lua $HOME/z.lua/z.lua --init bash)"

[ $# -gt 0 ] && _zlua "$*" && return

dir=$(_zlua -l 2>&1 | fzf-tmux -p --reverse --inline-info | sed 's/^[0-9,.]* *//')

if [ ! -z "$dir" ]
then
  tmux neww -c $dir
fi
