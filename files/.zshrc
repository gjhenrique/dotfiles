ZSH=$HOME/.oh-my-zsh

ZSH_THEME="bira"

plugins=(git gem history-substring-search fasd vagrant mvn node npm tmux)

source $HOME/.functions
source $HOME/.aliases
source $HOME/.source

source $ZSH/oh-my-zsh.sh

source $HOME/.export
source $HOME/.langs
#setopt PROMPT_SUBST ; PS1='[%n@%m %c$(__git_ps1 " (%s)")]\$'
