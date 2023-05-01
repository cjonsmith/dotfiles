# Variables
export NOTES_HOME=$HOME/notes
export EDITOR=/usr/bin/vim
# export BROWSER=$(command -v 'brave-browser' || command -v 'firefox')

# Path updates
[ -d /usr/local/bin ] && export PATH="/usr/local/bin:$PATH"
[ -d "$HOME/bin" ] && export PATH="$PATH:$HOME/bin"
[ -d "$HOME/.doom-emacs/bin" ] && export PATH="$PATH:$HOME/.doom-emacs/bin"
[ -d /usr/local/sbin ] && export PATH="/usr/local/sbin:$PATH"
2>&1 command -v go >/dev/null && export PATH="$(go env GOBIN):$PATH"

# Ignore commands starting with a space and a few other specific commands
export HISTIGNORE="[ ]*:&:bg:fg"

# Use cat as pager when using emacs M-x shell
[ "$TERM" = "dumb" ] && {
    alias less="cat"
    alias more="cat"
    export PAGER="cat"
    PS1="$ "
    EXCLUDED_FILES=( ! -name ps1 )
}

bash_config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/bash"

if [ -d "$bash_config_dir" ]; then
while IFS= read -r config_file; do
    # shellcheck source=/dev/null
    source "$config_file"
    done< <(find -H "$bash_config_dir" -type f "${EXCLUDED_FILES[@]}" -print)

    # shellcheck source=/dev/null
    touch "$bash_config_dir"/bash_hidden && source "$bash_config_dir"/bash_hidden
fi

# shellcheck source=/dev/null
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export NVM_DIR="$HOME/.nvm"
if [ -d "$NVM_DIR" ]; then
    # shellcheck source=/dev/null
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    # shellcheck source=/dev/null
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi
