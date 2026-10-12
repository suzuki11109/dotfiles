ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

zi snippet OMZL::history.zsh
zi snippet OMZL::completion.zsh
zi snippet OMZP::common-aliases
zi snippet OMZP::git
zi light zdharma-continuum/fast-syntax-highlighting
zi light hlissner/zsh-autopair
zi light zsh-users/zsh-autosuggestions
zi light zsh-users/zsh-completions

autoload -Uz compinit
compinit
zi cdreplay -q

setopt hist_reduce_blanks
setopt auto_cd

PS1='%F{blue}%~ %(?.%F{green}.%F{red})%#%f '

precmd_functions+=(set_terminal_title)
set_terminal_title() {
    print -Pn "\e]2;%-3~\a"
}

export CLICOLOR=1
export LSCOLORS="ExFxCxDxBxegedabagacad"

alias dot='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

alias vi="nvim"
alias vim="nvim"

alias kubectl="kubecolor"
alias k="kubecolor"
compdef kubecolor=kubectl

alias gfi='git flow init'
alias gfr='git flow release list'
alias gfh='git flow hotfix list'

gffr() {
    git flow release finish "$1" \
        --no-ff
        --update-message "Merge tag '$1' into %b"
}

gffh() {
    git flow hotfix finish "$1" \
        --no-ff
        --update-message "Merge tag '$1' into %b"
}

eval "$(zoxide init zsh)"
eval "$(atuin init zsh --disable-up-arrow)"
eval "$("$HOME/.local/bin/mise" activate zsh)"
eval "$(direnv hook zsh)"
