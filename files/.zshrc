ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

export LANGUAGE=en_US.UTF8
export LANG=en_US.UTF8
export LC_ALL=en_US.UTF8

plugins=(git gem history-substring-search fasd vagrant mvn node npm docker)

source $HOME/.functions
source $ZSH/oh-my-zsh.sh
source $HOME/.export
source $HOME/.langs
