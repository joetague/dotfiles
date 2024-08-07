# Set PATH, MANPATH, etc., for Homebrew.
# Skip: eval "$(/opt/homebrew/bin/brew shellenv)" use output explicitly below for perf reasons
export HOMEBREW_PREFIX="/opt/homebrew"
export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
export HOMEBREW_REPOSITORY="/opt/homebrew"
export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:"
export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}"
export HOMEBREW_NO_INSTALL_CLEANUP=TRUE

export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}"
export PATH="/opt/homebrew/opt/uutils-coreutils/libexec/uubin:$PATH"
export PATH="/opt/homebrew/opt/uutils-diffutils/libexec/uubin:$PATH"
export PATH="/opt/homebrew/opt/uutils-findutils/libexec/uubin:$PATH"
export PATH="/opt/homebrew/opt/curl/bin:$PATH"
export PATH="/opt/homebrew/opt/gawk/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/jpeg/bin:$PATH"
export PATH="/opt/homebrew/opt/sqlite/bin:$PATH"
export PATH="$HOME/.jenv/bin:$PATH"
export PATH="$HOME/proj/apache-maven-3.9.6/bin:$PATH"
export PATH="/opt/homebrew/opt/rustup/bin:$PATH"

export XDG_CONFIG_HOME="$HOME"/.config
export XDG_DATA_HOME="$HOME"/.local/share
export XDG_CACHE_HOME="$HOME"/.cache

export LDFLAGS="-L/opt/homebrew/opt/curl/lib"
export LDFLAGS="-L/opt/homebrew/opt/jpeg/lib $LDFLAGS"
export LDFLAGS="-L/opt/homebrew/opt/zlib/lib $LDFLAGS"

export CPPFLAGS="-I/opt/homebrew/opt/curl/include"
export CPPFLAGS="-I/opt/homebrew/opt/jpeg/include $CPPFLAGS"
export CPPFLAGS="-I/opt/homebrew/opt/zlib/include $CPPFLAGS"

export PKG_CONFIG_PATH="/opt/homebrew/opt/curl/lib/pkgconfig"
export PKG_CONFIG_PATH="/opt/homebrew/opt/jpeg/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/opt/homebrew/opt/zlib/lib/pkgconfig:$PKG_CONFIG_PATH"

# Rust lang setup
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup

export EMACS_SOCKET_NAME="${TMPDIR}/emacs$(id -u)/server"
export EDITOR="${EDITOR} --socket-name ${EMACS_SOCKET_NAME}"

eval "$(starship init zsh)"

[ -s "/opt/homebrew/opt/jabba/share/jabba/jabba.sh" ] && . "/opt/homebrew/opt/jabba/share/jabba/jabba.sh"

if which jenv > /dev/null; then eval "$(jenv init -)"; fi
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

export PATH="/Users/joetague/Library/Caches/fnm_multishells/56669_1686339042811/bin":$PATH
export FNM_DIR="/Users/joetague/Library/Application Support/fnm"
export FNM_MULTISHELL_PATH="/Users/joetague/Library/Caches/fnm_multishells/56669_1686339042811"
export FNM_VERSION_FILE_STRATEGY="local"
export FNM_LOGLEVEL="info"
export FNM_NODE_DIST_MIRROR="https://nodejs.org/dist"
export FNM_ARCH="arm64"
rehash

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

eval "$(zoxide init --cmd cd zsh)"
eval "$(fzf --zsh)"
eval "$(direnv hook zsh)"

## History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.history"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data

#
# Aliases
# (sorted alphabetically)
#
# Easier navigation: .., ..., ...., ....., ~ and -
alias -g ..="cd .."
alias -g ...="cd ../.."
alias -g ....="cd ../../.."
alias -g .....="cd ../../../.."
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"
alias bubc="brew upgrade && brew cleanup"
alias bubo="brew update && brew outdated"
alias bubu="bubo && bubc"
alias cat="bat"
alias cp="cp -irv"
alias df="df -H"
alias du="du -sh"
alias less="less --ignore-case --raw-control-chars"
alias l="eza -l"
alias ls="eza --classify --group --git"
alias ll='eza -alh'
alias tree='eza --tree'
alias make="nice make"
alias mkdir="mkdir -vp"
alias mv="mv -iv"

alias rm="rm -iv"
alias rsync="rsync --partial --progress --human-readable --compress"
alias sha256="shasum -a 256"

alias gss="git status --short"
alias gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify --no-gpg-sign --message "--wip-- [skip ci]"'
alias gunwip='git rev-list --max-count=1 --format="%s" HEAD | grep -q "\--wip--" && git reset HEAD~1'
alias gbrl="git for-each-ref --color=always --sort=-committerdate --format='%(color:yellow)%(refname:short)%09%(color:green)%(authorname)%09%(color:blue)%(committerdate:relative)%09%(color:red)%(objectname:short)%09%(color:magenta)%(upstream:track)%09%(color:white)%(contents:subject)%(color:reset)' refs/heads refs/remotes|column -t -s $'\t'"
alias gcount='git shortlog --summary --numbered'
alias glg='git log --stat'
alias glgp='git log --stat --patch'
alias glgg='git log --graph'
alias glgga='git log --graph --decorate --all'
alias glgm='git log --graph --max-count=10'
alias glo='git log --oneline --decorate'
alias glol="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'"
alias glols="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --stat"
alias glod="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'"
alias glods="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset' --date=short"
alias glola="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --all"
alias glog='git log --oneline --decorate --graph'
alias gloga='git log --oneline --decorate --graph --all'
