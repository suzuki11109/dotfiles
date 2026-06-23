eval "$(/opt/homebrew/bin/brew shellenv)"
export XDG_CONFIG_HOME=$HOME/.config
export PATH=$HOME/.local/bin:$PATH
export PATH=/usr/local/go/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/flutter/bin:$PATH
export PATH=$HOME/.pub-cache/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH
# export PATH=$HOME/.local/share/mise/shims:$PATH
export PNPM_HOME="/Users/aki/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME/bin:"*) ;;
  *) export PATH="$PNPM_HOME/bin:$PATH" ;;
esac

export JAVA_HOME=$(/usr/libexec/java_home -v 21)
export PATH=$JAVA_HOME/bin:$PATH

export KUBECONFIG=$HOME/.kube/config
export PATH="$PATH:$HOME/.lmstudio/bin"
export PATH="$PATH:/Applications/Emacs.app/Contents/MacOS/bin"
export PATH="$PATH:$HOME/Library/Application Support/Coursier/bin"

[ -f ~/.zprofile.local ] && source ~/.zprofile.local
