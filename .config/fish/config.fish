fish_vi_key_bindings

if status --is-login
	# Path updates
	set -gx PATH $PATH $HOME/bin
	set -gx PATH $PATH $HOME/Software/depot_tools

	# Variables
	set -gx NOTES_HOME $HOME/notes
	set -gx EDITOR /usr/bin/vim

	if test -z "$DISPLAY" -a $XDG_VTNR = 1
		exec startx -- -keeptty
	end
end

abbr dotfiles 'git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
abbr k        'kubectl'
abbr open     'xdg-open'

# Config files
abbr cf  'cd $HOME/.config ;and ls'
abbr cff 'cd $HOME/.config/fish ;and ls'
abbr cfm 'cd $HOME/.xmonad ;and ls'
abbr cfv 'cd $HOME/.vim ;and ls'

# Directory movements
abbr ws  'cd $HOME/Workspace ;and ls'
abbr bin 'cd $HOME/bin ;and ls'
abbr d   'cd $HOME/Documents ;and ls'
abbr D   'cd $HOME/Downloads ;and ls'
abbr n   'cd $NOTES_HOME ;and ls'
abbr p   'cd $HOME/Pictures ;and ls'
abbr bgs 'cd $HOME/Pictures/Backgrounds ;and ls'
abbr ac  'cd $APPCENTER ;and ls'
abbr s   'cd $HOME/Workspace/secrets ;and ls'
abbr so  'cd $HOME/Software ;and ls'

