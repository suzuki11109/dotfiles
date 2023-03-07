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
  zgenom load Aloxaf/fzf-tab
  zgenom load zdharma-continuum/fast-syntax-highlighting
  zgenom load zsh-users/zsh-completions
  zgenom load zsh-users/zsh-history-substring-search
  zgenom load hlissner/zsh-autopair
  zgenom load romkatv/powerlevel10k powerlevel10k
  zgenom load unixorn/fzf-zsh-plugin
  zgenom load agkozak/zsh-z
  zgenom load djui/alias-tips
  zgenom save
fi

# disable sort when completing options of any command
zstyle ':completion:complete:*:options' sort false

zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
	'git diff $word | delta'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
	'git log --color=always $word'
zstyle ':fzf-tab:complete:git-help:*' fzf-preview \
	'git help $word | bat -plman --color=always'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
	'case "$group" in
	"commit tag") git show --color=always $word ;;
	*) git show --color=always $word | delta ;;
	esac'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
	'case "$group" in
	"modified file") git diff $word | delta ;;
	"recent commit object name") git show --color=always $word | delta ;;
	*) git log --color=always $word ;;
	esac'

alias vi="nvim"
alias vim="nvim"
export EDITOR="nvim"

alias dot='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias k="kubectl"
alias gad="git fza"

# export DISABLE_AUTO_TITLE="true"
export PATH=$PATH:$HOME/.emacs.d/bin
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin
export PATH=$PATH:$HOME/.local/share/coursier/bin

export GOPATH=$HOME/go
export XDG_CONFIG_HOME=$HOME/.config
export PATH="$HOME/.local/share/fnm:$PATH"
# fzf colors
export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

command -v fnm >/dev/null && eval "`fnm env`"

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[[ -r "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

[[ -r "$HOME/emacs-vterm-zsh.sh" ]] && source "$HOME/emacs-vterm-zsh.sh"

[[ -r "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# comment if not install
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

eval "$(~/.rbenv/bin/rbenv init - zsh)"



# Load Angular CLI autocompletion.
source <(ng completion script)

executables(){
    echo -n "$PATH" | xargs -d: -I{} -r -- find -L {} -maxdepth 1 -mindepth 1 -type f -executable -printf '%P\n' 2>/dev/null | sort -u
}
