# dotfiles
Configs and dotfiles

## Setting up on New Machine

```bash
cd
git clone --separate-git-dir=$HOME/.dotfiles https://github.com/cjonsmith/dotfiles.git dotfiles-tmp
rsync --recursive --verbose --exclude '.git' --exclude 'README.md' dotfiles-tmp/.* $HOME/
rm --recursive dotfiles-tmp
source .bashrc
dotfiles config status.showUntrackedFiles no
```

## Usage

```bash
dotfiles status
dotfiles add .bashrc
dotfiles commit -m "Add bashrc"
dotfiles push
```
