ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

zi light zdharma-continuum/fast-syntax-highlighting
zi light zsh-users/zsh-completions
zi light hlissner/zsh-autopair
zi snippet OMZP::git

PS1='%F{blue}%~ %(?.%F{green}.%F{red})%#%f '

autoload -Uz compinit && compinit

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=10000
setopt sharehistory
setopt hist_ignore_all_dups
setopt hist_ignore_space

setopt extended_glob
setopt auto_cd
setopt auto_pushd
setopt interactive_comments
setopt menu_complete

export CLICOLOR=1
export LSCOLORS="ExFxCxDxBxegedabagacad"
export LS_COLORS="di=01;34:ln=01;36:ex=01;32:fi=00"
zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' cache-path ~/.zcompcache
zstyle ':completion:*' use-cache yes

bindkey -e
bindkey '^[[Z' reverse-menu-complete

# fuzzy

alias ls="ls -G"
alias ll="ls -alG"
alias dot='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias vi="nvim"
alias vim="nvim"
alias kubectl="kubecolor"
alias k="kubecolor"

compdef kubecolor=kubectl

eval "$(/Users/aki/.local/bin/mise activate zsh)"
command -v direnv >/dev/null 2>&1 && eval "$(direnv hook zsh)"

precmd () { print -Pn "\e]2;%-3~\a"; }
