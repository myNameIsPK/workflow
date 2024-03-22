# Create Workflow
``` bash
alias dot="git --git-dir=$HOME/.dotfile.git/ --work-tree=$HOME"
echo ".dotfile.git" >> .gitignore
git clone --bare https://github.com/pk-kampanart/workflow.git $HOME/.dotfile.git
rm -f .bash* && dot checkout
dot config --local status.showUntrackedFiles no
pacman -S git-crypt
dot crypt unlock
```

## Use Emacs

```bash
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d```
