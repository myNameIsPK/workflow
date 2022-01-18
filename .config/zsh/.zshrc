#!/bin/zsh
#    ███████╗███████╗██╗  ██╗██████╗  ██████╗
#    ╚══███╔╝██╔════╝██║  ██║██╔══██╗██╔════╝
#      ███╔╝ ███████╗███████║██████╔╝██║     
#     ███╔╝  ╚════██║██╔══██║██╔══██╗██║     
# ██╗███████╗███████║██║  ██║██║  ██║╚██████╗
# ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝

autoload -U colors && colors
PS1="%B%{$fg[red]%}[%{$fg[white]%}%n%{$fg[red]%}@%{$fg[white]%}%M %{$fg[blue]%}%c%{$fg[red]%}]%{$fg[green]%}$%b%{$reset_color%}"
[ -f "$ZDOTDIR/zsh-git-prompt" ] && source "$ZDOTDIR/zsh-git-prompt"
PS1+="%b "

# Load aliases
shortcuts-gen > /dev/null 2>&1
# [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases"

# Move history in xdg-cache
HISTSIZE=10000000
SAVEHIST=10000000
# HISTFILE=$HOME/.local/state/bash/history

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## SET VI MODE ###
bindkey -v
export KEYTIMEOUT=0

stty stop undef		# Disable ctrl-s to freeze terminal.

## SETOPT
setopt autocd		# Automatically cd into typed directory.
setopt interactive_comments
setopt histignoredups	# Ignore duplicate in history
unsetopt beep	# disable bell-noise

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

# Emacs keys in vi-insert mode
# FIXME: swap to emacs not work
# select-emacs() { set -o emacs }
# zle -N select-emacs
# bindkey -M viins '[e' select-emacs
bindkey -M viins '^A' beginning-of-line
bindkey -M viins '^E' end-of-line
bindkey -M viins '^R' history-incremental-search-backward
bindkey -M viins '^S' history-incremental-search-forward
bindkey -M viins '^F' forward-char
bindkey -M viins '^B' backward-char
bindkey -M viins '^D' delete-char-or-list
#bindkey -M viins '^K' kill-line
bindkey -M viins '^N' down-line-or-history
bindkey -M viins '^P' up-line-or-history

# Bash like Emacs mode
autoload edit-command-line; zle -N edit-command-line
bindkey '^X^E' edit-command-line
bindkey -M emacs '^X^E' edit-command-line

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

# ctrl+<- | ctrl+->
bindkey "^[[1;5D" backward-word
bindkey "^[[1;5C" forward-word

# TODO :edit hard code shortcut
## Shortcut
bindkey -s '^X^F' "^U_fzf-findfiles^M"

## Plugin
# Function to source files if they exist
function zsh_add_file() {
    [ -f "$ZDOTDIR/$1" ] && source "$ZDOTDIR/$1"
}

function zsh_add_plugin() {
    PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
    if [ -d "$ZDOTDIR/plugins/$PLUGIN_NAME" ]; then 
        # For plugins
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh" || \
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"
    else
        git clone "https://github.com/$1.git" "$ZDOTDIR/plugins/$PLUGIN_NAME"
    fi
}

zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"
