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

while IFS= read -r config_file; do
    source "$config_file"
done< <(find -H "${XDG_CONFIG_HOME:-$HOME/.config}/bash" -type f "${EXCLUDED_FILES[@]}" -print)

touch ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bash_hidden && source ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bash_hidden

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

if [ -n "$GITHUB_TOKEN" ]; then
  grep "machine goproxy.githubapp.com login nobody password $GITHUB_TOKEN" $HOME/.netrc || echo "machine goproxy.githubapp.com login nobody password $GITHUB_TOKEN" >> $HOME/.netrc
fi

export NVM_DIR="$HOME/.nvm"
if [ -d $NVM_DIR ]; then
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi

