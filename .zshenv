export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export SDL_IM_MODULE=fcitx
export GLFW_IM_MODULE=ibus

# hide user@host in title
export DISABLE_AUTO_TITLE="true"

export XDG_CONFIG_HOME=$HOME/.config
export PATH=$PATH:$HOME/.emacs.d/bin
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin

# export PATH="$HOME/.local/share/fnm:$PATH" # installed by pacman
command -v fnm >/dev/null && eval "`fnm env`"

command -v rbenv > /dev/null && eval "$(rbenv init - zsh)"

export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null && eval "$(pyenv init -)" && eval "$(pyenv virtualenv-init -)"

[[ -r "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"

command -v direnv >/dev/null && eval "$(direnv hook zsh)"

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
