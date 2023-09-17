#!/bin/zsh
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
unsetopt beep	# disable bell-noise
setopt autocd		# Automatically cd into typed directory.
setopt interactive_comments
setopt histignoredups	# Ignore duplicate in history
setopt incappendhistory # append entered command to history, don't wait for shell exit
setopt histignorespace # ignore command start with whitespace
setopt correctall
setopt combining_chars # fix unicode

## Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select

# Ignore case when auto complete
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' '+m:{A-Z}={a-z}'
zmodload zsh/complist
# compinit # move to last line
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
bindkey -M viins '^K' kill-line
bindkey -M viins '^N' down-line-or-history
bindkey -M viins '^P' up-line-or-history

# Home/End
bindkey -M viins '^[[1~' beginning-of-line
bindkey -M viins '^[[4~' end-of-line

# Delete
bindkey -M viins '^[[3~' delete-char-or-list
 
# ctrl+<- | ctrl+->
bindkey "^[[1;5D" backward-word
bindkey "^[[1;5C" forward-word

# Bash like Emacs mode
autoload edit-command-line; zle -N edit-command-line
bindkey '^X^E' edit-command-line
bindkey -M emacs '^X^E' edit-command-line

# TODO :edit hard code shortcut
# Shortcut
bindkey -s '^X^X' "^Ustartx^M"

bindkey -s '^X^S' "^Utmux-ses^M"

# Change cursor shape for different vi modes.
cursor_mode() {
  cursor_block='\e[2 q'
  cursor_beam='\e[6 q'

  function zle-keymap-select {
    if [[ ${KEYMAP} == vicmd ]] ||
        [[ $1 = 'block' ]]; then
        echo -ne $cursor_block
    elif [[ ${KEYMAP} == main ]] ||
        [[ ${KEYMAP} == viins ]] ||
        [[ ${KEYMAP} = '' ]] ||
        [[ $1 = 'beam' ]]; then
        echo -ne $cursor_beam
    fi
  }

  zle-line-init() {
    echo -ne $cursor_beam
  }

  zle -N zle-keymap-select
  zle -N zle-line-init
}

cursor_mode

# Plugin
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
zsh_add_plugin "zsh-users/zsh-completions"
zsh_add_plugin "hlissner/zsh-autopair"
# zsh_add_plugin "MenkeTechnologies/zsh-expand"
fpath=("$ZDOTDIR/plugins/zsh-completions" $fpath)
compinit

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh

# Modified from Guix auto bash config
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${match[1]} [guix env]\\\$ "
    fi
fi

# bun completions
[ -s "/home/pk/.bun/_bun" ] && source "/home/pk/.bun/_bun"
