export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export SDL_IM_MODULE=fcitx
export GLFW_IM_MODULE=ibus

export EDITOR="nvim"
export XDG_CONFIG_HOME=$HOME/.config
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin
export KUBECONFIG=$HOME/.kube/config

command -v direnv > /dev/null && eval "$(direnv hook bash)"

command -v fnm > /dev/null && eval "`fnm env`"

command -v rbenv > /dev/null && eval "$(rbenv init - bash)"

[[ -r "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
command -v pyenv >/dev/null && eval "$(pyenv init -)" && eval "$(pyenv virtualenv-init -)"

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

[[ -f ~/.bashrc ]] && . ~/.bashrc
