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

# export PATH="$HOME/.rbenv/bin:$PATH"
command -v rbenv > /dev/null && eval "$(rbenv init - zsh)"

export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null && eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"

export PATH=$PATH:$HOME/.local/share/coursier/bin

[[ -r "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

command -v pyenv >/dev/null && eval "$(direnv hook zsh)"

# >>> JVM installed by coursier >>>
export JAVA_HOME="$HOME/.cache/coursier/arc/https/github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u292-b10/OpenJDK8U-jdk_x64_linux_hotspot_8u292b10.tar.gz/jdk8u292-b10"
export PATH="$PATH:$HOME/.cache/coursier/arc/https/github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u292-b10/OpenJDK8U-jdk_x64_linux_hotspot_8u292b10.tar.gz/jdk8u292-b10/bin"
# <<< JVM installed by coursier <<<

