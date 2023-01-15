if status --is-login
	# Variables
	set -gx NOTES_HOME $HOME/notes
	set -gx EDITOR /usr/bin/vim
	set -gx APPCENTER $HOME/Workspace/work/appcenter
	set -gx ANDROID_HOME $HOME/Library/Android/sdk
	set -gx BROWSER firefox
	set -gx ENVIRONMENTS $HOME/Environments

	# Path updates
	set locations \
		$HOME/.rbenv/bin \
		/snap/bin \
		$APPCENTER/diagnostics/utilities/bin \
		$HOME/Software/depot_tools \
		$HOME/.cargo/bin \
		$ANDROID_HOME/emulator \
		$ANDROID_HOME/tools \
		$ANDROID_HOME/tools/bin \
		$ANDROID_HOME/platform-tools \
		$HOME/Workspace/jit/bin

	for location in $locations
		if test -d $location
			set -gx PATH $PATH $location
		end
	end

	if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
		exec startx -- -keeptty
	end

	# Download and install fisher
	if not functions -q fisher
		set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
		curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
		fish -c fisher
	end
end

source $HOME/.config/fish/abbreviations.fish

if command --search rbenv
	status --is-interactive; and source (rbenv init -|psub)
end
