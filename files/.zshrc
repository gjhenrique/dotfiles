source $HOME/.zsh_keybindings
source $HOME/.git-prompt.sh
source $HOME/.functions
source $HOME/.aliases

WORK_FILE=$HOME/Life/work.sh

setopt hist_ignore_all_dups

if [ -f $WORK_FILE ];
then
    source $WORK_FILE
fi

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
