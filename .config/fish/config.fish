fish_vi_key_bindings

if status --is-login
	# Path updates
	set -x PATH $PATH ~/bin

	# Variables
	set -x NOTES_HOME $HOME/notes
	set -x EDITOR /usr/bin/vim

	if test -z "$DISPLAY" -a $XDG_VTNR = 1
		exec startx -- -keeptty
	end
end

abbr dotfiles 'git --git-dir=$HOME/.dotfiles --work-tree=$HOME'

# Config files
abbr cf  'cd $HOME/.config ;and ls'
abbr cff 'cd $HOME/.config/fish ;and ls'
abbr cfm 'cd $HOME/.xmonad ;and ls'

# Directory movements
abbr ws  'cd $HOME/Workspace ;and ls'
abbr bin 'cd $HOME/bin ;and ls'
abbr d   'cd $HOME/Documents ;and ls'
abbr D   'cd $HOME/Downloads ;and ls'
abbr n   'cd $NOTES_HOME ;and ls'
abbr p   'cd $HOME/Pictures ;and ls'
abbr bgs 'cd $HOME/Pictures/Backgrounds ;and ls'

