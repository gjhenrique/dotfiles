ZSH=$HOME/.oh-my-zsh

ZSH_THEME="bira"

plugins=(git gem history-substring-search fasd vagrant mvn node npm debian)

source $ZSH/oh-my-zsh.sh

source $HOME/.git-prompt.sh
source $HOME/.functions
source $HOME/.aliases
source $HOME/.export
source $HOME/.langs
