function pyenv --description 'Small wrapper script to source virtual environments'
	set -l usage \
		(string join '' 'Usage: ' (tput bold) 'pyenv (-e env | --environment=env)')

	set -l options \
		(fish_opt --short=e --long=environment --required) \
		(fish_opt --short=h --long=help)


	argparse $options -- $argv

	if test -n "$_flag_help"
		echo $usage
		return
	end

	if test -z "$_flag_environment"
		echo (tput bold)"ERROR: -e/--environment flag not found!"(tput sgr0)
		echo $usage
		return 1
	end

	source $HOME/Environments/$_flag_environment/bin/activate.fish
end

