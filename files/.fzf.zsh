# Setup fzf
# ---------
if [[ ! "$PATH" == */home/guilherme/.fzf/bin* ]]; then
  export PATH="$PATH:$HOME/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "$HOME/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "$HOME/.fzf/shell/key-bindings.zsh"

export FZF_DEFAULT_COMMAND='ag -l -g ""'
