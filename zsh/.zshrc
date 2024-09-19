# Set PATH, MANPATH, etc., for Homebrew.
# Skip: eval "$(/opt/homebrew/bin/brew shellenv)" use output explicitly below for perf reasons
export HOMEBREW_PREFIX="/opt/homebrew"
export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
export HOMEBREW_REPOSITORY="/opt/homebrew"
export MANPATH="$HOMEBREW_PREFIX/share/man${MANPATH+:$MANPATH}:"
export INFOPATH="$HOMEBREW_PREFIX/share/info:${INFOPATH:-}"
export HOMEBREW_NO_INSTALL_CLEANUP=TRUE

# Hoping some CLI apps respect these between Darwin (MacOS) and Linux
# https://specifications.freedesktop.org/basedir-spec/latest/
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin${PATH+:$PATH}"
export PATH="$HOMEBREW_PREFIX/opt/curl/bin:$PATH"
export PATH="$HOMEBREW_PREFIX/opt/gawk/libexec/gnubin:$PATH"
export PATH="$HOMEBREW_PREFIX/opt/gnu-sed/libexec/gnubin:$PATH"
export PATH="$HOMEBREW_PREFIX/opt/jpeg/bin:$PATH"
export PATH="$HOMEBREW_PREFIX/opt/sqlite/bin:$PATH"
export PATH="$HOME/proj/apache-maven-3.9.6/bin:$PATH"
export PATH="$HOMEBREW_PREFIX/opt/rustup/bin:$PATH"

export LDFLAGS="-L/opt/homebrew/opt/curl/lib"
export LDFLAGS="-L/opt/homebrew/opt/jpeg/lib $LDFLAGS"
export LDFLAGS="-L/opt/homebrew/opt/zlib/lib $LDFLAGS"

export CPPFLAGS="-I/opt/homebrew/opt/curl/include"
export CPPFLAGS="-I/opt/homebrew/opt/jpeg/include $CPPFLAGS"
export CPPFLAGS="-I/opt/homebrew/opt/zlib/include $CPPFLAGS"

export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/curl/lib/pkgconfig"
export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/jpeg/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/zlib/lib/pkgconfig:$PKG_CONFIG_PATH"

# Rust lang setup
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

eval "$(starship init zsh)"
# Go lang setup
export GOPATH="$XDG_DATA_HOME/go"
export GOMODCACHE="$XDG_CACHE_HOME/go/mod"
export PATH="$GOPATH/bin:$PATH"

# Java lang setup
[ -s "$HOMEBREW_PREFIX/opt/jabba/share/jabba/jabba.sh" ] && . "$HOMEBREW_PREFIX/opt/jabba/share/jabba/jabba.sh"
export JENV_ROOT="$XDG_DATA_HOME/jenv"
if which jenv > /dev/null; then eval "$(jenv init -)"; fi
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"

export PYENV_ROOT="$XDG_DATA_HOME/pyenv"
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

# Move aspell config and personal dictionary
export ASPELL_CONF="per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_DATA_HOME/aspell/en.pws; repl $XDG_DATA_HOME/aspell/en.prepl"

export PATH="/Users/joetague/Library/Caches/fnm_multishells/56669_1686339042811/bin":$PATH
export FNM_DIR="/Users/joetague/Library/Application Support/fnm"
export FNM_MULTISHELL_PATH="/Users/joetague/Library/Caches/fnm_multishells/56669_1686339042811"
export FNM_VERSION_FILE_STRATEGY="local"
export FNM_LOGLEVEL="info"
export FNM_NODE_DIST_MIRROR="https://nodejs.org/dist"
export FNM_ARCH="arm64"
rehash

# Emacs related
export EMACS_SOCKET_NAME="${TMPDIR}/emacs$(id -u)/server"
export EDITOR="${EDITOR} --socket-name ${EMACS_SOCKET_NAME}"
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
[ -z "$HISTFILE" ] && HISTFILE="$XDG_STATE_HOME/zsh/history"
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
