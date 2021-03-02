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

    # notify when a long running command finishes.
    # https://github.com/marzocchi/zsh-notify
    notify
)

kubectl() {
    # Lazy load kubectl completion.
    # Normally we'd add kubectl to the plugins list, but it adds 0.1s to the startup time.
    # See https://github.com/ohmyzsh/ohmyzsh/issues/6843#issuecomment-414193880
    unfunction "$0"
    source <(kubectl completion zsh)
    $0 "$@"
}

ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

if [ "$ZSH" = "~/.oh-my-zsh" ]; then
    # Auto update.
    DISABLE_UPDATE_PROMPT=true
else
    # Completely disable if installed system-wide (e.g. like on Arch from AUR).
    DISABLE_AUTO_UPDATE=true
fi

source $ZSH/oh-my-zsh.sh

if [[ -n "$INSIDE_EMACS" ]]; then
    # Emacs leaks escape sequences with ZSH and xterm-256color.
    # This TERM value seems to get rid of the leaked escape sequences and preserve color.
    export TERM=eterm-color
else
    export TERM=xterm-256color
fi

[ -n "$TMUX" ] && export TERM=screen-256color

source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Shell History
source $ZSH/lib/history.zsh

# When searching for history entries in the line editor, do not display
# duplicates of a line previously found, even if the duplicates are not
# contiguous.
setopt hist_find_no_dups

alias clip='xsel --clipboard --input'
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

# pyenv.
export PATH="/home/danmilon/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Platform.sh stuff:
# Automatically added by the Platform.sh CLI
export PATH="/home/danmilon/.magento-cloud/bin:$PATH"
export PATH="/home/danmilon/.platformsh/bin:$PATH"
. '/home/danmilon/.platformsh/shell-config.rc' 2>/dev/null

alias platform-tb='PLATFORMSH_CLI_ACCOUNTS_API=http://testbed.plat.farm platform'
alias platform-staging='PLATFORMSH_CLI_SESSION_ID=staging \
PLATFORMSH_CLI_API_URL=https://api.staging.plat.farm \
PLATFORMSH_CLI_ACCOUNTS_API=https://accounts.staging.plat.farm/api/v1/ \
PLATFORMSH_CLI_OAUTH2_AUTH_URL=https://auth.plat.farm/oauth2/authorize \
PLATFORMSH_CLI_OAUTH2_TOKEN_URL=https://auth.plat.farm/oauth2/token \
PLATFORMSH_CLI_OAUTH2_REVOKE_URL=https://auth.plat.farm/oauth2/revoke \
PLATFORMSH_CLI_CERTIFIER_URL=https://ssh.auth.plat.farm \
PLATFORMSH_CLI_SSH_DOMAIN_WILDCARD=*.plat.farm \
platform'
