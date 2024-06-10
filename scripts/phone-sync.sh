#!/usr/bin/env bash

set -euo pipefail

echo -n "check programs: "
if command -v ssh >/dev/null && command -v rsync >/dev/null; then
    echo "ok"
else
    echo "not ok"
    exit 1
fi

PHONE_SERVER="phone"
echo "checking phone server "
echo -n "remote user: "
if ! ssh $PHONE_SERVER "whoami"; then
    echo "error cannot ssh"
    echo "ssh status: erro"
    exit 1
else
    echo "ssh status: fine"
fi

echo "===Sync: notes"
rsync -avz ~/notes/ phone:notes
echo "===Sync: Emacs"
ssh phone "cd ~/.config/nvim; git pull"
echo "===Sync: Nvim"
ssh phone "cd ~/.config/emacs; git pull"
echo "===Sync: Dotfiles"
ssh phone "GIT_DIR=~/.dotfile.git GIT_WORK_TREE=\$HOME bash -c 'git reset --hard && git pull -f'"
