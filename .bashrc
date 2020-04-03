# Variables
export NOTES_HOME=$HOME/notes
export APPCENTER=$HOME/Workspace/appcenter
export EDITOR=/usr/bin/vim

# Path updates
[ -d /snap/bin ] && export PATH="/snap/bin:$PATH"
[ -d /usr/local/bin ] && export PATH="/usr/local/bin:$PATH"
[ -d "$HOME/bin" ] && export PATH="$PATH:$HOME/bin"
[ -d "$APPCENTER" ] && export PATH="$PATH:$APPCENTER/diagnostics/utilities/bin"
[ -d "$HOME/Software/depot_tools" ] && export PATH="$PATH:$HOME/Software/depot_tools"

for file in ${XDG_CONFIG_HOME:-$HOME/.config}/bash/*; do
	source "$file"
done

touch ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bash_hidden && source ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bash_hidden

stty stop ^J # Bind `stop` to CTRL-J so forward-search history is available

export NVM_DIR="$HOME/.nvm"
if [ -d $NVM_DIR ]; then
	[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
	[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
