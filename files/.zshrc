source $HOME/.zsh_keybindings
source $HOME/.git-prompt.sh
source $HOME/.functions
source $HOME/.aliases

WORK_FILE=$HOME/Dropbox/Vida/work.sh

if [ -f $WORK_FILE ];
then
    source $WORK_FILE
fi
