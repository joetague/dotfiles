# ~/.zshrc
# Interactive shells. Environment (PATH, Homebrew, build flags, EDITOR) is in
# ~/.zprofile; agent detection (DISABLE_SHELL_TOOLING) is in ~/.zshenv.

#GnuPG
export GPG_TTY=$TTY

## History file configuration
# /etc/zshrc has already set HISTFILE=~/.zsh_history, so assign unconditionally
HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history"
[[ -d ${HISTFILE:h} ]] || mkdir -p ${HISTFILE:h}
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data

# Everything below is for humans only. Skipped when an agent sets
# DISABLE_SHELL_TOOLING, so commands behave as stock and output stays plain.
[[ -n "$DISABLE_SHELL_TOOLING" ]] && return

#
# Aliases
# (sorted alphabetically)
#
# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
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
fpath=("$HOME/.docker/completions" $fpath)
fpath=("${HOMEBREW_PREFIX:-/opt/homebrew}/share/zsh/site-functions" $fpath)
autoload -Uz compinit
# /usr/bin/stat: GNU coreutils' stat is earlier on PATH and reads -f differently
if [ "$(date +'%j')" != "$(/usr/bin/stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)" ]; then
    compinit
else
    compinit -C
fi

eval "$(mise activate zsh)"
eval "$(fzf --zsh)"
eval "$(starship init zsh)"
eval "$(op completion zsh)"; compdef _op op
eval "$(zoxide init --cmd cd zsh)"
[[ -r $HOME/.config/op/plugins.sh ]] && source "$HOME/.config/op/plugins.sh"

# Machine-local interactive setup (aliases, functions); untracked.
# Environment that agents also need goes in ~/.zprofile.local instead.
[[ -r $HOME/.zshrc.local ]] && source "$HOME/.zshrc.local"
