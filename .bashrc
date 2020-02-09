# Variables
export NOTES_HOME=$HOME/notes
export APPCENTER=$HOME/Workspace/appcenter
export EDITOR=/usr/bin/vim

# Path updates
export PATH="/snap/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$APPCENTER/diagnostics/utilities/bin"
export PATH="$PATH:$HOME/Software/depot_tools"

source $HOME/.bash_autocomplete	# Autocompletes
source $HOME/.ps1				# PS1 (prompt)
source $HOME/.bash_aliases		# Aliases
source $HOME/.bash_functions	# Functions

touch $HOME/.bash_hidden && source $HOME/.bash_hidden

stty stop ^J # Bind `stop` to CTRL-J so forward-search history is available

export NVM_DIR="$HOME/.nvm"
if [ -d $NVM_DIR ]; then
	[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
	[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
