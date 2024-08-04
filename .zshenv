export EDITOR="nvim"
export XDG_CONFIG_HOME=$HOME/.config
export KUBECONFIG=$HOME/.kube/config
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:/usr/local/go1.22.5/bin
export PATH=$PATH:$HOME/go/bin
export PATH=$HOME/flutter/bin:$PATH

eval "$(/opt/homebrew/bin/brew shellenv)"

alias ls="ls --color"
alias vi="nvim"
alias vim="nvim"
alias k="kubectl"
alias dot='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

command -v nodenv >/dev/null && eval "$(nodenv init -)"
command -v rbenv >/dev/null && eval "$(pyenv init -)"
command -v rbenv >/dev/null && eval "$(rbenv init -)"

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
