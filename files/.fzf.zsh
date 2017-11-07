# Setup fzf
# ---------
if [[ ! "$PATH" == */home/guilherme/.fzf/bin* ]]; then
  export PATH="$PATH:/home/guilherme/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/guilherme/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/guilherme/.fzf/shell/key-bindings.zsh"

