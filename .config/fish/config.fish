fish_vi_key_bindings

if status --is-login
	# Variables
	set -gx NOTES_HOME $HOME/notes
	set -gx EDITOR /usr/bin/vim
	set -gx APPCENTER $HOME/Workspace/work/appcenter
	set -gx ANDROID_HOME $HOME/Library/Android/sdk
	set -gx BROWSER firefox
	set -gx ENVIRONMENTS $HOME/Environments

	# Path updates
	set -gx PATH $PATH $HOME/bin
	set -gx PATH $PATH $APPCENTER/diagnostics/utilities/bin
	set -gx PATH $PATH $HOME/Software/depot_tools
	set -gx PATH $PATH $HOME/.cargo/bin
	set -gx PATH $PATH $ANDROID_HOME/emulator
	set -gx PATH $PATH $ANDROID_HOME/tools
	set -gx PATH $PATH $ANDROID_HOME/tools/bin
	set -gx PATH $PATH $ANDROID_HOME/platform-tools

	if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
		exec startx -- -keeptty
	end
end

abbr dotfiles 'git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
abbr k        'kubectl'
abbr dcb      '~/Workspace/work/appcenter/dockercompose/build.ps1'
abbr dcr      '~/Workspace/work/appcenter/dockercompose/restart.ps1'
abbr dct      '~/Workspace/work/appcenter/dockercompose/tests.ps1'
abbr dcf      '~/Workspace/work/appcenter/dockercompose/functional.ps1'
abbr dcps     '~/Workspace/work/appcenter/dockercompose/ps.ps1'
abbr dcc      '~/Workspace/work/appcenter/dockercompose/clean.ps1'
abbr dcs      '~/Workspace/work/appcenter/dockercompose/stop.ps1'

# Config files
abbr cf   'cd $HOME/.config ;and ls'
abbr cff  'cd $HOME/.config/fish ;and ls'
abbr cfff 'cd $HOME/.config/fish/functions ;and ls'
abbr cfm  'cd $HOME/.xmonad ;and ls'
abbr cfv  'cd $HOME/.vim ;and ls'
abbr cfb  'cd $HOME/.vim/bundle ;and ls'

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

status --is-interactive; and . (rbenv init -|psub)
