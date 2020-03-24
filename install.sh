#!/usr/bin/env sh
dotfiles_dir=$(basedir "$0")
rsync --recursive --exclude '.git' "$dotfiles_dir/*" "$HOME"
ln -sf "$HOME/.config/vim/vimrc" "$HOME/.vimrc"
# shellcheck source=./.bashrc
. "$HOME/.bashrc"
dotfiles config status.showUntrackedFiles no
dotfiles submodule update --init

