# ~/.zprofile
# Login shells: environment only. Interactive setup lives in ~/.zshrc;
# non-login shells (ghostel, nested zsh) inherit this from their parent.

# XDG base directories
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Set PATH, MANPATH, etc., for Homebrew.
# Skip: eval "$(/opt/homebrew/bin/brew shellenv)" use output explicitly below for perf reasons
export HOMEBREW_DOWNLOAD_CONCURRENCY=auto
export HOMEBREW_PREFIX="/opt/homebrew"
export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
export HOMEBREW_REPOSITORY="/opt/homebrew"
export HOMEBREW_NO_INSTALL_CLEANUP=TRUE

# Keep path-like variables free of duplicates. -U only dedupes on array
# assignment, so build them through the arrays below.
typeset -U path fpath manpath
typeset -TUx INFOPATH infopath

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
  "$HOMEBREW_PREFIX/opt/rustup/bin"
  "$HOME/.local/bin"
  "$HOME/.docker/bin"
  "$HOME/.cargo/bin"
  "$HOME/.lmstudio/bin"
  $path
)
# Trailing empty element keeps man/info's default search path
manpath=("$HOMEBREW_PREFIX/share/man" "${manpath[@]}" "")
infopath=("$HOMEBREW_PREFIX/share/info" "${infopath[@]}" "")
export MANPATH

export LDFLAGS="-L/opt/homebrew/opt/openssl@3/lib -L/opt/homebrew/opt/curl/lib -L/opt/homebrew/opt/jpeg/lib -L/opt/homebrew/opt/zlib/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openssl@3/include -I/opt/homebrew/opt/curl/include -I/opt/homebrew/opt/jpeg/include -I/opt/homebrew/opt/zlib/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/sqlite/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl@3/lib/pkgconfig:/opt/homebrew/opt/curl/lib/pkgconfig:/opt/homebrew/opt/jpeg/lib/pkgconfig:/opt/homebrew/opt/zlib/lib/pkgconfig:/opt/homebrew/opt/readline/lib/pkgconfig:/opt/homebrew/opt/sqlite/lib/pkgconfig"
export SSL_CERT_FILE="/opt/homebrew/etc/openssl@3/cert.pem"

# Move aspell config and personal dictionary
export ASPELL_CONF="per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_DATA_HOME/aspell/en.pws; repl $XDG_DATA_HOME/aspell/en.prepl"

# Emacs related (agents get EDITOR=true from ~/.zshenv)
export EMACS_SOCKET_NAME="${TMPDIR}/emacs${UID}/server"
[[ -z "$DISABLE_SHELL_TOOLING" ]] && export EDITOR="emacsclient -c --socket-name ${EMACS_SOCKET_NAME}"

# Agent client settings
export OPENCODE_DISABLE_LSP_DOWNLOAD=true

# Machine-local environment (internal hosts, proxies, tokens); untracked.
# Login shells only, so agents get it too. Interactive extras: ~/.zshrc.local.
[[ -r $HOME/.zprofile.local ]] && source "$HOME/.zprofile.local"

# Last, so mise shims come before Homebrew. mise prepends to the PATH scalar,
# which -U ignores, so reassign the array to drop a repeated shims entry.
eval "$(/opt/homebrew/bin/mise activate zsh --shims)"
path=($path)
