[[ -f ~/.bashrc ]] && . ~/.bashrc

tf='/usr/local/bin/terraform'
[[ -f "$tf" ]] && complete -C "$tf" terraform
