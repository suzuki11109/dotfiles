export GOROOT=$HOME/go
export GOPATH=$HOME/code/go
# export GO111MODULE=on
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
export FZF_DEFAULT_COMMAND="rg --files --hidden --follow"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export ZSH="/Users/aki/.oh-my-zsh"
ZSH_THEME="robbyrussell"
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export PATH=/usr/local/miniconda3/bin:$PATH
export PATH="$PATH:$HOME/.rvm/bin"
export ASPNETCORE_ENVIRONMENT=Development
export XDG_CONFIG_HOME=$HOME/.config

plugins=(fast-syntax-highlighting zsh-autopair zsh-completions)
autoload -U compinit && compinit

source $ZSH/oh-my-zsh.sh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(fasd --init auto)"

alias vi="nvim"
alias vim="nvim"
alias ls="ls --color=auto"

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/mysql-client/bin:$PATH"
