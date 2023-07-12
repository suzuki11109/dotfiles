# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source "${HOME}/.zgenom/zgenom.zsh"
zgenom autoupdate
if ! zgenom saved; then
  zgenom ohmyzsh
  zgenom ohmyzsh plugins/git
  zgenom load zdharma-continuum/fast-syntax-highlighting
  zgenom load zsh-users/zsh-completions
  zgenom load zsh-users/zsh-history-substring-search
  zgenom load hlissner/zsh-autopair
  zgenom load romkatv/powerlevel10k powerlevel10k
  zgenom load djui/alias-tips
  zgenom save
fi

alias vi="nvim"
alias vim="nvim"
export EDITOR="nvim"

alias dot='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias k="kubectl"

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[[ -r "$HOME/emacs-vterm-zsh.sh" ]] && source "$HOME/emacs-vterm-zsh.sh"

# Load Angular CLI autocompletion.
command -v ng >/dev/null && source <(ng completion script)

[[ -r "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

eval "$(zoxide init zsh)"
