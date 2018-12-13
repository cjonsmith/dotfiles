function acclean {
	local current_directory=$(pwd)
	cd ~/Workspace/appcenter
	git clean -xdf
	cd $current_directory
}

