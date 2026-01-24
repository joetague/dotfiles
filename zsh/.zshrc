# Set PATH, MANPATH, etc., for Homebrew.
# Skip: eval "$(/opt/homebrew/bin/brew shellenv)" use output explicitly below for perf reasons
export HOMEBREW_DOWNLOAD_CONCURRENCY=auto
export HOMEBREW_PREFIX="/opt/homebrew"
export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
export HOMEBREW_REPOSITORY="/opt/homebrew"
export MANPATH="$HOMEBREW_PREFIX/share/man${MANPATH+:$MANPATH}:"
export INFOPATH="$HOMEBREW_PREFIX/share/info:${INFOPATH:-}"
export HOMEBREW_NO_INSTALL_CLEANUP=TRUE

#GnuPG
GPG_TTY=$TTY
export GPG_TTY

# Hoping some CLI apps respect these between Darwin (MacOS) and Linux
# https://specifications.freedesktop.org/basedir-spec/latest/
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

path=(
  "$HOMEBREW_PREFIX/bin"
  "$HOMEBREW_PREFIX/sbin"
  "$HOMEBREW_PREFIX/opt/curl/bin"
  "$HOMEBREW_PREFIX/opt/gawk/libexec/gnubin"
  "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin"
  "$HOMEBREW_PREFIX/opt/gnu-sed/libexec/gnubin"
  "$HOMEBREW_PREFIX/opt/grep/libexec/gnubin"
  "$HOMEBREW_PREFIX/opt/jpeg/bin"
  "$HOMEBREW_PREFIX/opt/sqlite/bin"
  "$HOME/.local/bin"
  "$HOME/.docker/bin"
  "$HOME/.cargo/bin"
  "$HOME/.lmstudio/bin"
  $path
)

export LDFLAGS="-L/opt/homebrew/opt/openssl@3/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openssl@3/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl@3/lib/pkgconfig"
export SSL_CERT_FILE="/opt/homebrew/etc/openssl@3/cert.pem"
export REQUESTS_CA_BUNDLE="/opt/homebrew/etc/openssl@3/cert.pem"

#export LDFLAGS="-L/opt/homebrew/opt/curl/lib"
#export LDFLAGS="-L/opt/homebrew/opt/jpeg/lib $LDFLAGS"
#export LDFLAGS="-L/opt/homebrew/opt/zlib/lib $LDFLAGS"

#export CPPFLAGS="-I/opt/homebrew/opt/curl/include"
#export CPPFLAGS="-I/opt/homebrew/opt/jpeg/include $CPPFLAGS"
#export CPPFLAGS="-I/opt/homebrew/opt/zlib/include $CPPFLAGS"

#export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/curl/lib/pkgconfig"
#export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/jpeg/lib/pkgconfig:$PKG_CONFIG_PATH"
#export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/zlib/lib/pkgconfig:$PKG_CONFIG_PATH"

# Move aspell config and personal dictionary
export ASPELL_CONF="per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_DATA_HOME/aspell/en.pws; repl $XDG_DATA_HOME/aspell/en.prepl"

# Emacs related
export EMACS_SOCKET_NAME="${TMPDIR}/emacs$(id -u)/server"
export EDITOR="emacsclient -c --socket-name ${EMACS_SOCKET_NAME}"
# export EDITOR="nano"
# If we’re in an SSH session but NOT inside tmux, attach or create a session
if [[ -n "$SSH_TTY" && -z "$TMUX" && -o interactive ]] && command -v tmux >/dev/null; then
    tmux_session="ssh_${USER}_$(hostname -s)"
    tmux attach-session -t "$tmux_session" 2>/dev/null || tmux new-session -s "$tmux_session"
fi

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

# Start SSH agent if not running
# if [ -z "$SSH_AUTH_SOCK" ]; then
#     eval "$(ssh-agent -s)" > /dev/null
#     ssh-add ~/.ssh/id_github_sign_and_auth 2>/dev/null
# fi

eval "$(mise activate zsh)"
eval "$(fzf --zsh)"
eval "$(zoxide init --cmd cd zsh)"
eval "$(starship init zsh)"

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
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
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
alias k9s="k9s --readonly"

# Smarter completion initialization
fpath=(/Users/joetague/.docker/completions $fpath)
fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
autoload -Uz compinit
if [ "$(date +'%j')" != "$(stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)" ]; then
    compinit
else
    compinit -C
fi
eval "$(op completion zsh)"; compdef _op op
