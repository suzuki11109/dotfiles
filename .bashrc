if [ -f ~/bash-sensible/sensible.bash ]; then
   source ~/bash-sensible/sensible.bash
fi

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export SDL_IM_MODULE=fcitx
export GLFW_IM_MODULE=ibus
export EDITOR="nvim"
export XDG_CONFIG_HOME=$HOME/.config
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin
export PATH=$PATH:/home/aki/.local/share/coursier/bin
export KUBECONFIG=$HOME/.kube/config

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"

alias vi="nvim"
alias vim="nvim"
alias dot='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias k="kubectl"
alias ls="ls --color"

# export PYENV_ROOT="$HOME/.pyenv"
# export PATH="$PYENV_ROOT/bin:$PATH"
# command -v pyenv >/dev/null && eval "$(pyenv init -)" && eval "$(pyenv virtualenv-init -)"

# export PATH="$HOME/.rbenv/bin:$PATH"
# command -v rbenv >/dev/null && eval "$(rbenv init - bash)"

# export PATH="$HOME/.nodenv/bin:$PATH"
# command -v nodenv >/dev/null && eval "$(nodenv init - bash)"

# export SDKMAN_DIR="$HOME/.sdkman"
# [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# [[ -r "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

# command -v yarn >/dev/null && export PATH="$PATH:$(yarn global bin)"


[[ -s "$HOME/.bashrc.local" ]] && source "$HOME/.bashrc.local"

[[ $- != *i* ]] && return

command -v direnv > /dev/null && eval "$(direnv hook bash)"

# command -v starship > /dev/null && eval "$(starship init bash)"
