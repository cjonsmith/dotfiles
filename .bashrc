# Update path to local scripts
export PATH="/snap/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/Workspace/appcenter/diagnostics/utilities/bin"

# Add system variable for notes directory
export NOTES_HOME="$HOME/notes"
export APPCENTER=~/Workspace/appcenter

source $HOME/.bash_autocomplete	# Autocompletes
source $HOME/.ps1				# PS1 (prompt)
source $HOME/.bash_aliases		# Aliases
source $HOME/.bash_functions	# Functions

