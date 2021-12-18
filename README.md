# Create Workflow
```
alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME"
echo ".dotfiles.git" >> .gitignore
git clone --bare https://github.com/myNameIsPK/workflow.git $HOME/.dotfiles.git
rm -f .bash* && dotfiles checkout
dotfiles config --local status.showUntrackedFiles no
```
