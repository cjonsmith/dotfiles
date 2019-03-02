fish_vi_key_bindings

if status --is-login
	# Path updates
	set -x PATH $PATH ~/bin

	# Variables
	set -x NOTES_HOME $HOME/notes
	set -x EDITOR /usr/bin/vim
end

abbr dotfiles 'git --git-dir=$HOME/.dotfiles --work-tree=$HOME'

# Config files
abbr cf 'cd $HOME/.config ;and ls'
abbr cff 'cd $HOME/.config/fish ;and ls'

# Directory movements
abbr ws 'cd $HOME/Workspace'
