autoload -U zmv

export ZSH=~/.oh-my-zsh/
export ZSH_CUSTOM=~/.zsh/custom

export DEFAULT_USER="danmilon"
ZSH_THEME="my-bira"
plugins=(
    gitfast
    pip
    vagrant
    urltools
    sudo
    python
    pass
    kubectl
)

ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_AUTO_UPDATE="true"

source $ZSH/oh-my-zsh.sh

export TERM=xterm-256color
[ -n "$TMUX" ] && export TERM=screen-256color

source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $ZSH/lib/history.zsh

alias clip='xclip -selection clipboard'
alias o='xdg-open'
alias less='less -R'
alias updateall='sudo yaourt --noconfirm -Syu'
# hide emacs tmp files
alias ls='ls --color -F --hide="*~" --hide="#*#"'
alias cp='cp -i'
alias here='nautilus ./ > /dev/null 2>&1 &'
alias heret='$TERMINAL . > /dev/null 2>&1 &'

# browsers
alias chromium='chromium --enable-webgl --no-referrers'
alias firefox='VDPAU_DRIVER=va_gl firefox'

export SUDO_EDITOR="emacsclient -t"
alias e="$SUDO_EDITOR"
alias se='sudoedit'
alias ew="emacsclient -c"
export EDITOR=emacsclient
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=100000
export HISTFILESIZE=100000

setopt appendhistory

export GOPATH=~/.go
PATH=$GOPATH/bin:$PATH
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.gem/ruby/2.2.0/bin
export PATH

PYMACS_PYTHON=python2

# autoload emacs daemon with emacsclient
export ALTERNATE_EDITOR=""

source ~/.zsh/z.sh

alias grep='grep $GREP_OPTIONS'
unset GREP_OPTIONS

alias rm="echo Use 'trash'"
alias atc-from-internet='(cd ~/atc-archive && vlc --rate=1.5 $(rsync -arv -e "ssh root@home.danmilon.me ssh danmilon@rtlsdr-atc-1.danmilon.node.her.wn" :/home/danmilon/atc-archives/ ~/atc-archive/  | tee -a /dev/tty ~/tmp/atc | sort-atc-archives))'
alias atc-from-home='(cd ~/atc-archive && vlc --rate=1.5 $(rsync -arv -e "ssh danmilon@rtlsdr-atc-1.danmilon.node.her.wn" :/home/danmilon/atc-archives/ ~/atc-archive/  | tee -a /dev/tty ~/tmp/atc | sort-atc-archives))'


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
    echo $py_version
    cd $(mktemp -d)
    virtualenv -p python$py_version venv
    . venv/bin/activate
    for pkg in "$@"; do

    done

    if [ -n "$2" ]; then
	pip install ipython $(echo $2)
    fi

    venv/bin/ipython
}

# fzf
source /usr/share/fzf/shell/key-bindings.zsh

# prompt
PROMPT="%B${return_code}%b
${user_host} ${current_dir}%B${user_symbol}%b $ "
RPS1=""

# Platform.sh stuff:
# Automatically added by the Platform.sh CLI
export PATH="/home/danmilon/.platformsh/bin:$PATH"
. '/home/danmilon/.platformsh/shell-config.rc' 2>/dev/null

alias platform-tb='PLATFORMSH_CLI_ACCOUNTS_API=http://testbed.plat.farm platform'

# undistract-me
source /usr/share/undistract-me/long-running.bash
# For some reason the following function exits with code 1.
# Until I find out why, put this at the end of the file.
notify_when_long_running_commands_finish_install
