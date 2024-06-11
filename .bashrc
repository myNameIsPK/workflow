# vi:fdm=marker ft=sh:
# shellcheck disable=2148,2139

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## Test function {{{
# function is_zsh(){ [[ $(readlink /proc/$$/exe) == */zsh ]] }
# function is_bash(){ [[ $(readlink /proc/$$/exe) == */bash ]] }
function is_zsh(){ [ -n "${ZSH_VERSION}" ]; }
function is_bash(){ [ -n "${BASH_VERSION}" ]; }
function source_if_exist(){ if [ -f $1 ]; then source $1; fi }
# }}}

## Prompt {{{
if is_zsh; then
    ## Zsh: prompt{{{
    # default PS1='[%n@%M %c]\$ '
    rst="%{$(tput sgr0)%}"
    # blk="%{$(tput setaf 0)%}"
    red="%{$(tput setaf 1)%}"
    gre="%{$(tput setaf 2)%}"
    # yel="%{$(tput setaf 3)%}"
    blu="%{$(tput setaf 4)%}"
    # mgt="%{$(tput setaf 5)%}"
    # cya="%{$(tput setaf 6)%}"
    # whi="%{$(tput setaf 7)%}"
    PS1="${red}[${rst}%n${red}@${rst}%M ${blu}%c${red}]${gre}\$${rst}"
    PS1+="%b "
    #}}}
elif is_bash; then
    ## Bash: prompt{{{
    # default PS1='[\u@\h \W]\$ '
    rst="\[$(tput sgr0)\]"
    # blk="\[$(tput setaf 0)\]"
    red="\[$(tput setaf 1)\]"
    gre="\[$(tput setaf 2)\]"
    # yel="\[$(tput setaf 3)\]"
    blu="\[$(tput setaf 4)\]"
    # mgt="\[$(tput setaf 5)\]"
    # cya="\[$(tput setaf 6)\]"
    # whi="\[$(tput setaf 7)\]"
    PS1="${red}[${rst}\u${red}@${rst}\h ${blu}\W${red}]${gre}\$${rst} "
    # }}}
fi
# }}}

## Load shortcuts{{{
shortcuts-gen > /dev/null 2>&1
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
# }}}

## Zsh: history{{{
# Move history in xdg-cache
HISTSIZE=10000000
SAVEHIST=10000000
# HISTFILE=$HOME/.local/state/bash/history
# }}}
## Bash: history{{{
HISTCONTROL=ignoreboth	# Ignore duplicate in history 
# }}}

## Zsh: SET VI MODE{{{
if is_zsh; then
    bindkey -v
    export KEYTIMEOUT=0
fi
# }}}

stty stop undef # Disable ctrl-s to freeze terminal.

## Zsh: set options{{{
if is_zsh; then
    unsetopt beep # disable bell-noise
    setopt autocd # Automatically cd into typed directory.
    setopt interactive_comments
    setopt histignoredups # Ignore duplicate in history
    setopt incappendhistory # append entered command to history, don't wait for shell exit
    setopt histignorespace # ignore command start with whitespace
    setopt correctall
    setopt combining_chars # fix unicode
    setopt auto_pushd # Push the current directory visited on the stack.
    setopt pushd_ignore_dups # Do not store duplicates in the stack.
    # setopt pushd_silent # Do not print the directory stack after pushd or popd.
fi
# }}}
## Bash: set options{{{
if is_bash; then
    shopt -s autocd		# Auto cd to directory
    shopt -s cdspell	# Auto correct dirctory name
    shopt -s cmdhist	# Save multi-line commands in history as single
    shopt -s dotglob	# Includ dot file in globbling
    shopt -s histappend	# Append history not overwrite when exit
    shopt -s checkwinsize	# Checks term size when bash regains control
fi
# }}}

## Zsh: completions {{{
if is_zsh; then
    autoload -U compinit
    zstyle ':completion:*' menu select
    # Ignore case when auto complete
    zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' '+m:{A-Z}={a-z}'
    # Groups
    zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
    # Group completion
    zstyle ':completion:*' group-name ''
    # Group type order
    zstyle ':completion:*:*:-command-:*:*' group-order alias builtins functions commands
    zmodload zsh/complist
    # compinit # move to last line
    _comp_options+=(globdots) # Include hidden files.
fi #}}}

## Zsh: bindkeys and cursor{{{
# shellcheck disable=2296
if is_zsh; then
    # Vim text-object
    autoload -Uz select-bracketed select-quoted
    zle -N select-quoted
    zle -N select-bracketed
    for km in viopp visual; do
        bindkey -M $km -- '-' vi-up-line-or-history
        for c in {a,i}${(s..)^:-\'\"\`\|,./:;=+@}; do
            bindkey -M $km $c select-quoted
        done
        for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
            bindkey -M $km $c select-bracketed
        done
    done

    # vim-surround
    autoload -Uz surround
    zle -N delete-surround surround
    zle -N add-surround surround
    zle -N change-surround surround
    bindkey -M vicmd cs change-surround
    bindkey -M vicmd ds delete-surround
    bindkey -M vicmd ys add-surround
    bindkey -M visual S add-surround

    # Use vim keys in tab complete menu:
    bindkey -M menuselect 'h' vi-backward-char
    bindkey -M menuselect 'k' vi-up-line-or-history
    bindkey -M menuselect 'l' vi-forward-char
    bindkey -M menuselect 'j' vi-down-line-or-history

    # Emacs keys in vi-insert mode
    switch-emacs() { set -o emacs; }
    switch-vi() { set -o vi; }
    zle -N switch-emacs
    zle -N switch-vi
    bindkey -M viins '^X^Z' switch-emacs
    bindkey -M viins '\ee' switch-emacs
    bindkey -M vicmd '^X^Z' switch-emacs
    bindkey -M vicmd '\ee' switch-emacs
    bindkey -M emacs '^X^Z' switch-vi
    bindkey -M emacs '\ev' switch-vi
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
    bindkey -M viins '\e.' insert-last-word
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
fi # }}}
## Zsh: Plugins{{{
if is_zsh; then
    PLUGIN_DIR="${ZDOTDIR:-$HOME/zsh}"
    [ ! -d $PLUGIN_DIR/plugins ] && mkdir -p "$PLUGIN_DIR/plugins"
    function zsh_add_file() {
        [ -f "$PLUGIN_DIR/$1" ] && source "$PLUGIN_DIR/$1"
    }
    function zsh_add_plugin() {
        PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
        PLUGIN_PATH="$PLUGIN_DIR/plugins/$PLUGIN_NAME"
        if [ ! -d "$PLUGIN_PATH" ]; then
            git clone "https://github.com/$1.git" "$PLUGIN_DIR/plugins/$PLUGIN_NAME"
        fi
        if [ -d "$PLUGIN_PATH" ]; then 
            zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh" || \
                zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"
        fi
    }

    zsh_add_plugin "zsh-users/zsh-autosuggestions"
    zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
    zsh_add_plugin "zsh-users/zsh-completions"
    zsh_add_plugin "hlissner/zsh-autopair"
    # zsh_add_plugin "MenkeTechnologies/zsh-expand"
    fpath=("$PLUGIN_DIR/plugins/zsh-completions" $fpath)
    compinit
fi
# }}}

## Alias and Function{{{
alias a=alias

[ -f "$XINITRC" ] && alias startx="startx $XINITRC"

# Shortcut: Reload config file
alias rebash="source $HOME/.config/bash/bashrc"
alias rezsh="source $HOME/.config/zsh/.zshrc"
alias cfq="$MY_SRC/my_qmk"

# Tmux
alias ta="tmux a"
alias tls="tmux ls"
alias tks="tmux kill-sess"
alias tkp="tmux kill-pane"
alias td="tmux detach-client"

alias ts="~/bin/tmux-ses"

# Git shortcut
alias g="git"
alias gs="git status"
alias gss="git status --short"
alias ga="git add"
alias gaa="git add --all --verbose"
alias gc="git commit"
alias gca="git commit --amend"
alias gu="git rm --cached" 
alias gd="git restore" 
alias grs="git reset" 
alias gst="git stash" 
alias gb="git branch"
alias gba="git branch --all"
alias gbd="git branch --delete"
alias gbD="git branch -D"
alias gco="git checkout"
alias gw="git worktree"
alias gwl="git worktree list"
alias gwa="git worktree add"
alias gwd="git worktree remove"
alias grb="git rebase"
alias grbi="git rebase -i"
alias grba="git rebase --abort"
alias grbc="git rebase --continue"
alias gm="git merge"
alias gmt="git mergetool"
alias gp="git pull"
alias gP="git push"
alias gPf="git push --force-with-lease"
alias gsr="git remote set-url origin"
alias grv="git remote -v"
alias gls="git ls-files"
alias ggraph="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"

# Packages
pksize () {
    pacman -Qi \
        | awk '/^Name/{name=$3} /^Installed Size/{print $4$5, name}' \
        | sort -h \
        | fzf --tac --no-sort --multi --preview 'pacman -Qi $(echo {} | cut -d" " -f2)'
}

pklist () {
    pacman -Qqe | fzf --multi --preview 'pacman -Qi {}'
}

pkcantremove () {
    pacman -Qei \
        | awk '\
            /^Name/{name=$3}\
            /^Required By/{if ($4 != "None")\
                {name=name" "NF-3;\
                for (i=4;i<=NF;i++){name=name" "$i}print name}}'
}

# Colorize output
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

# Confirm file operation
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -vi"

# Utilities
alias vi="nvim"
alias viss="nvim -S $XDG_CONFIG_HOME/nvim/tmp.session"
alias vig="nvim +Neogit"
alias dot="git --git-dir=$DOTFILES --work-tree=$HOME"
alias dotcd="GIT_DIR=$DOTFILES GIT_WORK_TREE=$HOME $SHELL"
alias dotenv="GIT_DIR=$DOTFILES GIT_WORK_TREE=$HOME"

alias em="emacsclient -t -a ''" # this autostart emacs daemon
alias emd="emacs --daemon"
alias emk="pkill emacs"
alias emls="emacsclient -t -a '' --eval \"(dired \\\"\$(pwd)\\\")\""
alias emg="emacsclient -t -a '' --eval \"(magit \\\"\$(pwd)\\\")\""
alias doom="emacs -nw --with-profile doom"

alias psg="ps aux | rg "

manfzf () {
    man -k . | fzf | cut -d' ' -f1 | xargs -I{} man {}
}

infofzf () {
    info "$(info -k . | fzf | sed "s#\"\(.*\)\"\s*--.*#\1#")"
}

# ls Flags
alias l="ls -CF"
alias ll="ls -lAFrt"
alias la="ls -lAF --group-directories-first"
alias lh="ls -lAFhS"

# Human readable
alias df="df -h"
alias free="free -m"
alias duh="du -h --max-depth=1 | sort -h -r"

# Hack
alias cd-="cd -"
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias :q=exit
# End: Alias and Function}}}

# fzf shortcuts 
if command -v fzf > /dev/null ; then
    is_zsh && source_if_exist "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh
    is_bash && source_if_exist "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash
fi

# starship prompt
if command -v starship > /dev/null ; then
    is_bash && eval "$(starship init bash)"
    is_zsh && eval "$(starship init zsh)"
fi

# bun completions
is_zsh && source_if_exist "/home/pk/.bun/_bun"

# pipx completions
if is_zsh; then
    autoload -U bashcompinit
    bashcompinit
    eval "$(register-python-argcomplete pipx)"
fi

# direnv
if command -v direnv > /dev/null ; then
    is_bash && eval "$(direnv hook bash)"
    is_zsh && eval "$(direnv hook zsh)"
fi
