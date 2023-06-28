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
  zgenom load unixorn/fzf-zsh-plugin
  zgenom load Aloxaf/fzf-tab
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

#fzf-tab
zstyle ':completion:*:git-checkout:*' sort false

export FZF_DEFAULT_OPTS=" \
--info=inline --no-sort --keep-right --height=40% \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[[ -r "$HOME/emacs-vterm-zsh.sh" ]] && source "$HOME/emacs-vterm-zsh.sh"

# Load Angular CLI autocompletion.
command -v ng >/dev/null && source <(ng completion script)

[[ -r "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

export _ZO_FZF_OPTS="--no-sort --keep-right --height=40% --layout=reverse \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

eval "$(zoxide init zsh)"
bindkey -s '^j' 'zi^M'
# bindkey -s '^j' 'cd $(zoxide query -l | fzf --reverse --inline-info)^M'

# source $HOME/fzf-tab-completion/zsh/fzf-zsh-completion.sh
# bindkey '^I' fzf_completion

v ()
{
  __zoxide_zi
  nvim
}
