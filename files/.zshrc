ZSH=$HOME/.oh-my-zsh

ZSH_THEME="bira"

plugins=(git gem history-substring-search fasd vagrant mvn node npm)

source $ZSH/oh-my-zsh.sh

source $HOME/.zsh_keybindings
source $HOME/.git-prompt.sh
source $HOME/.functions
source $HOME/.aliases
source $HOME/.langs

WORK_FILE=$HOME/Dropbox/Vida/work.sh
if [ -f $WORK_FILE ];
then
    source $WORK_FILE
fi

