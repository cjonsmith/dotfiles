function bright --description 'Adjust brightness of display'
	set brightness_file /sys/class/backlight/intel_backlight/brightness
	set options \
		(fish_opt --short 'i' --long 'increase' --optional-val) \
		(fish_opt --short 'd' --long 'decrease' --optional-val) \
		(fish_opt --short 's' --long 'set' --required-val) \
		(fish_opt --short 'h' --long 'help' --optional-val)

	if test -z $argv[1]
		printf '%s\n' (cat $brightness_file)
		return 0
	end

	argparse $options -- $argv

	if set -q _flag_help
		printf '%sbright%s usage:\n\n' (tput bold) (tput sgr0)
		printf 'bright\n'
		printf '  Prints the current brightness level\n'
		printf 'bright [-i/--increase=][VALUE]\n'
		printf '  Increases the brightness by VALUE, defaults to 10\n'
		printf 'bright [-d/--decrease=][VALUE]\n'
		printf '  Decreases the brightness by VALUE, defaults to 10\n'
		printf 'bright [-s/--set] VALUE\n'
		printf '  Sets the brightness to VALUE, must be greater than 0\n'
		printf 'bright [-h/--help]\n'
		printf '  Prints this help message, then exits\n'
		return 0
	end

	if set -q _flag_set; and test $_flag_set -gt 0
		echo $_flag_set | sudo tee $brightness_file >> /dev/null
		return 0
	end

	if set -q _flag_increase
		if test -z $_flag_increase
			set _flag_increase 10
		end
		echo (math (bright) + $_flag_increase) | sudo tee $brightness_file >> /dev/null
	end

	if set -q _flag_decrease
		if test -z $_flag_decrease
			set _flag_decrease 10
		end
		echo (math (bright) - $_flag_decrease) | sudo tee $brightness_file >> /dev/null
	end
end

