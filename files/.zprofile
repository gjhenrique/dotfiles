WORK_FILE=$HOME/Life/work.sh
if [ -f $WORK_FILE ];
then
    source $WORK_FILE
fi

PC_SETTINGS_FILE=$HOME/.settings.sh
if [ -f $PC_SETTINGS_FILE ];
then
    source $PC_SETTINGS_FILE
fi

source $HOME/.export
source $HOME/.langs

WORK_FILE=$HOME/Life/work.sh
if [ -f $WORK_FILE ];
then
    source $WORK_FILE
fi
