ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

zi light zdharma-continuum/fast-syntax-highlighting
zi light zsh-users/zsh-completions
zi light hlissner/zsh-autopair
zi snippet OMZP::git

autoload -U compinit && compinit

zi cdreplay -q

bindkey -e

HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zsh_history
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_save_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_find_no_dups
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
bindkey '^[[Z' reverse-menu-complete

PS1='%F{blue}%~ %(?.%F{green}.%F{red})%#%f '

alias ls="ls --color"
alias vi="nvim"
alias vim="nvim"
# alias k="kubectl"
alias kubectl="kubecolor"
alias k="kubecolor"
alias dot='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

command -v rbenv >/dev/null && eval "$(pyenv init -)"
command -v rbenv >/dev/null && eval "$(rbenv init -)"
command -v direnv >/dev/null && eval "$(direnv hook zsh)"
