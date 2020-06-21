# Variables
export NOTES_HOME=$HOME/notes
export APPCENTER=$HOME/Workspace/appcenter
export EDITOR=/usr/bin/vim
export BROWSER=$(command -v 'brave-browser' || command -v 'firefox')

# Path updates
[ -d /usr/local/bin ] && export PATH="/usr/local/bin:$PATH"
[ -d "$HOME/bin" ] && export PATH="$PATH:$HOME/bin"

for file in ${XDG_CONFIG_HOME:-$HOME/.config}/bash/*; do
	source "$file"
done

touch ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bash_hidden && source ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bash_hidden

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export NVM_DIR="$HOME/.nvm"
if [ -d $NVM_DIR ]; then
	[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
	[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi

