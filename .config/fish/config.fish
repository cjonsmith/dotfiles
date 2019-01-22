fish_vi_key_bindings

if status --is-login
	# Path updates
	set -x PATH $PATH ~/bin

	# Variables
	set -x NOTES_HOME $HOME/notes
	set -x EDITOR /usr/bin/vim
end

abbr dotfiles 'git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
