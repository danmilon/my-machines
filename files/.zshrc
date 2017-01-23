autoload -U zmv

export ZSH=/usr/share/oh-my-zsh
export ZSH_CUSTOM=~/.zsh/custom

export DEFAULT_USER="danmilon"
ZSH_THEME="my-bira"
plugins=(gitfast pip vagrant urltools sudo python pass)
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_AUTO_UPDATE="true"

source $ZSH/oh-my-zsh.sh

export TERM=xterm-256color
[ -n "$TMUX" ] && export TERM=screen-256color

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

alias git='hub'
alias clip='xclip -selection clipboard'
alias o='xdg-open'
alias less='less -R'
alias updateall='sudo yaourt --noconfirm -Syu'
# hide emacs tmp files
alias ls='ls --color -F --hide="*~" --hide="#*#"'
alias cp='cp -i'
alias here='nautilus ./ > /dev/null 2>&1 &'
alias heret='gnome-terminal . > /dev/null 2>&1 &'

# browsers
alias chromium='chromium --enable-webgl --no-referrers'
alias firefox='VDPAU_DRIVER=va_gl firefox'

export SUDO_EDITOR="emacsclient -t"
alias e="$SUDO_EDITOR"
alias se='sudoedit'
alias ew="emacsclient -c"
export EDITOR=emacsclient
export TERMINAL=gnome-terminal
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=100000
export HISTFILESIZE=100000

setopt appendhistory
PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

export GOPATH=~/.go
PATH=$GOPATH/bin:$PATH
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.gem/ruby/2.2.0/bin
export PATH

PYMACS_PYTHON=python2

# autoload emacs daemon with emacsclient
export ALTERNATE_EDITOR=""

source /usr/share/z/z.sh
eval "$(rbenv init -)"

alias grep='grep $GREP_OPTIONS'
unset GREP_OPTIONS

alias rm="echo Use 'trash'"

function chpwd() {
    if [[ -f venv/bin/activate ]]; then
	. venv/bin/activate
    elif [[ -f .virtualenv/bin/activate ]]; then
	. .virtualenv/bin/activate
    elif [[ -f .env/bin/activate ]]; then
	. .env/bin/activate
    fi
}

function pytmp() {
    py_version=${1:-3}
    cd $(mktemp -d)
    virtualenv -p python$1 venv
    . venv/bin/activate
    for pkg in "$@"; do

    done

    if [ -n "$2" ]; then
	pip install $(echo $2)
    fi
}

# fzf
source /usr/share/fzf/key-bindings.zsh

# Automatically added by the Platform.sh CLI
export PATH="/home/danmilon/.platformsh/bin:$PATH"
. '/home/danmilon/.platformsh/shell-config.rc' 2>/dev/null
