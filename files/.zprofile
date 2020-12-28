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

if [[ $TERM == "dumb" ]]; then	# in emacs
    PS1='%(?..[%?])%!:%~%# '
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
else
    ZSH=$HOME/.oh-my-zsh
    ZSH_THEME="bira"
    plugins=(git gem history-substring-search systemd aws autojump kubectl)
    export SHOW_AWS_PROMPT=false
    source $ZSH/oh-my-zsh.sh
fi

source $HOME/.export
source $HOME/.langs
