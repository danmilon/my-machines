
PS1='\[\e[1;32m\][\W]$(__git_ps1 " (%s) ")\$\[\e[0m\] '

alias o='xdg-open'
alias less='less -R'
alias updateall='sudo yaourt --noconfirm -Syu'
alias ls='ls --color -F'
alias here='nautilus ./ > /dev/null 2>&1 &'
alias heret='gnome-terminal . > /dev/null 2>&1 &'
alias chromium='chromium --enable-webgl --no-referrers'
export EDITOR=emacsclient
export TERMINAL=gnome-terminal
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=100000
export HISTFILESIZE=100000
shopt -s histappend
PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

export GOPATH=~/.go
PATH=$GOPATH/bin:$PATH
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/.gem/ruby/2.0.0/bin
export PATH

# autoload emacs daemon with emacsclient
export ALTERNATE_EDITOR=""

[[ -s ~/.bash_secrets ]] && . ~/.bash_secrets
[[ -s /usr/share/git/git-prompt.sh ]] && . /usr/share/git/git-prompt.sh
