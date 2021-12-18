# Create Workflow
```
alias dot="/usr/bin/git --git-dir=$HOME/.dotfile.git/ --work-tree=$HOME"
echo ".dotfile.git" >> .gitignore
git clone --bare https://github.com/myNameIsPK/workflow.git $HOME/.dotfile.git
rm -f .bash* && dot checkout
dot config --local status.showUntrackedFiles no
```
