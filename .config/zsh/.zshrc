#!/bin/zsh
#    ███████╗███████╗██╗  ██╗██████╗  ██████╗
#    ╚══███╔╝██╔════╝██║  ██║██╔══██╗██╔════╝
#      ███╔╝ ███████╗███████║██████╔╝██║     
#     ███╔╝  ╚════██║██╔══██║██╔══██╗██║     
# ██╗███████╗███████║██║  ██║██║  ██║╚██████╗
# ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝

autoload -U colors && colors
PS1="%B%{$fg[red]%}[%{$fg[white]%}%n%{$fg[red]%}@%{$fg[white]%}%M %{$fg[blue]%}%~%{$fg[red]%}]%{$fg[green]%}$%b%{$reset_color%} %b"

# Load aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases"

# Move history in xdg-cache
HISTSIZE=10000000
SAVEHIST=10000000
# HISTFILE=$HOME/.local/state/bash/history

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## SET VI MODE ###
bindkey -v
export KEYTIMEOUT=1

stty stop undef		# Disable ctrl-s to freeze terminal.

## SETOPT
setopt autocd		# Automatically cd into typed directory.
setopt interactive_comments
setopt histignoredups	# Ignore duplicate in history

## Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
# Ignore case when auto complete
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' '+m:{A-Z}={a-z}'
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# Emacs keys in vi mode
bindkey -v '^A' beginning-of-line
bindkey -v '^E' end-of-line
bindkey -v '^R' history-incremental-search-backward

# ctrl+<- | ctrl+->
bindkey "^[[1;5D" backward-word
bindkey "^[[1;5C" forward-word

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Edit line in vim with ctrl-v:
autoload edit-command-line; zle -N edit-command-line
bindkey '^V' edit-command-line

# TODO :edit hard code shortcut
## Shortcut
# ctrl-o for 'la'
#bindkey -s '^O' 'la\n'
alias cfz='$EDITOR ~/.config/zsh/.zshrc'

## Starship prompt should be last
# eval "$(starship init zsh)"
