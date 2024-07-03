export EDITOR="nvim"
export XDG_CONFIG_HOME=$HOME/.config
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin
export KUBECONFIG=$HOME/.kube/config

alias vi="nvim"
alias vim="nvim"
alias k="kubectl"
alias dot='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# fcitx config
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export SDL_IM_MODULE=fcitx
export GLFW_IM_MODULE=ibus
