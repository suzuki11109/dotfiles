export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export SDL_IM_MODULE=fcitx
export GLFW_IM_MODULE=ibus

export XDG_CONFIG_HOME=$HOME/.config
export PATH=$PATH:$HOME/.emacs.d/bin
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin

# export PATH="$HOME/.local/share/fnm:$PATH" # installed by pacman
command -v fnm >/dev/null && eval "`fnm env`"

# if you install coursier
#export PATH=$PATH:$HOME/.local/share/coursier/bin

[[ -r "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"

# if you install sdkman
# export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# export PYENV_ROOT="$HOME/.pyenv"
# command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"

export PATH="$HOME/.rbenv/bin:$PATH"
command -v rbenv > /dev/null && eval "$(rbenv init - zsh)"
