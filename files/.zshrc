source $HOME/.zsh_keybindings
source $HOME/.git-prompt.sh
source $HOME/.functions
source $HOME/.aliases

WORK_FILE=$HOME/Dropbox/Vida/work.sh

setopt hist_ignore_all_dups

if [ -f $WORK_FILE ];
then
    source $WORK_FILE
fi
