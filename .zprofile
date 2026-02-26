eval "$(/opt/homebrew/bin/brew shellenv)"
export PATH=$HOME/.local/bin:$PATH
export XDG_CONFIG_HOME=$HOME/.config
export PATH=/usr/local/go1.23.7/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/flutter3.29.2/bin:$PATH
export PATH=$HOME/.pub-cache/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.pyenv/shims:$PATH
export PATH=$HOME/.rbenv/shims:$PATH
export PNPM_HOME="/Users/aki/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
export PATH=/opt/homebrew/opt/libpq/bin:$PATH
export JAVA_HOME=$HOME/jdk17.0.14
export PATH=$JAVA_HOME/bin:$PATH
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk

export KUBECONFIG=$HOME/.kube/config
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export PATH="$PATH:/Users/aki/.lmstudio/bin"

[ -f ~/.zprofile.local ] && source ~/.zprofile.local
