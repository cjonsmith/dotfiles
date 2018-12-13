# Update path to local scripts
export PATH="/snap/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/Workspace/appcenter/diagnostics/utilities/bin"

# Add system variable for notes directory
export NOTES_HOME="$HOME/notes"
export APPCENTER=~/Workspace/appcenter

## Start PS1 Changes
# Colors for PS1
COLOR_WHITE="\033[38;5;15m"
COLOR_YELLOW="\033[38;5;11m"
COLOR_CYAN="\033[38;5;14m"
COLOR_RED="\033[0;31m"
COLOR_GREEN="\033[0;32m"
COLOR_RESET="\033[0m"

# Git coloring courtesy of
# https://coderwall.com/p/pn8f0g/show-your-git-status-and-branch-in-color-at-the-command-prompt
function git_color {
	local git_status="$(git status 2> /dev/null)"

	if [[ ! $git_status =~ "working tree clean" ]]; then
		echo -e $COLOR_RED
	elif [[ $git_status =~ "nothing to commit" ]]; then
		echo -e $COLOR_GREEN
	else
		echo -e $COLOR_WHITE
	fi
}

function git_branch {
	local git_status="$(git status 2> /dev/null)"
	local on_branch="On branch ([^${IFS}]*)"
	local on_commit="HEAD detached at ([^${IFS}*])"

	if [[ $git_status =~ $on_branch ]]; then
		local branch=${BASH_REMATCH[1]}
		echo "($branch)"
	elif [[ $git_status =~ $on_commit ]]; then
		local commit=${BASH_REMATCH[1]}
		echo "($commit)"
	fi
}

function prev_func_err {
	if [[ $? -ne 0 ]]; then
		echo -e "💩 "
	fi
}
PS1="\$(prev_func_err)"
PS1+="\[$COLOR_YELLOW\]\u\[$COLOR_WHITE\] in "	# Username and `in`
PS1+="\[$COLOR_CYAN\]\W\[$COLOR_WHITE\]"		# Basename of dir
PS1+="\[\$(git_color)\]\$(git_branch)"			# Branch and color
PS1+="\[$COLOR_WHITE\]$ \[$COLOR_RESET\]"		# Trailing `$`
## End PS1 Changes

# Source .bash_aliases
source $HOME/.bash_aliases

function acclean {
	local current_directory=$(pwd)
	cd ~/Workspace/appcenter
	git clean -xdf
	cd $current_directory
}

