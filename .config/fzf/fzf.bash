# Setup fzf
# ---------
if [[ ! "$PATH" == */home/pk/.local/src/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/home/pk/.local/src/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/pk/.local/src/fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/pk/.local/src/fzf/shell/key-bindings.bash"
