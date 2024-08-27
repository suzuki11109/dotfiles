eval "$(/opt/homebrew/bin/brew shellenv)"

# export XDG_CONFIG_HOME=$HOME/.config
# export KUBECONFIG=$HOME/.kube/config

export PATH=$HOME/.local/bin:$PATH
export PATH=/usr/local/go1.22.5/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/flutter3.19.6/bin:$PATH

export PATH=$HOME/.pyenv/shims:$PATH
export PATH=$HOME/.rbenv/shims:$PATH
export PATH=$HOME/.nodenv/shims:$PATH

export PNPM_HOME="/Users/aki/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
