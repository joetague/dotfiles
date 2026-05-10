#!/usr/bin/env bash
set -euo pipefail

# Backup original value of a defaults key before overwriting.
# Saves to XDG_DATA_HOME so it is machine-local and not tracked in git.
# Subsequent runs are no-ops for keys already in the backup file.
_backup_dir="${XDG_DATA_HOME:-$HOME/.local/share}/macos-tweaks"
_backup_file="$_backup_dir/defaults-backup.txt"

backup_default() {
    local domain=$1
    local key=$2
    local lookup="${domain}|${key}"

    mkdir -p "$_backup_dir"

    # Already backed up — do not overwrite the original.
    if grep -Fq "${lookup}|" "$_backup_file" 2>/dev/null; then
        return
    fi

    local value
    if value=$(defaults read "$domain" "$key" 2>/dev/null); then
        printf '%s|%s\n' "$lookup" "$value" >> "$_backup_file"
    else
        printf '%s|__NOT_SET__\n' "$lookup" >> "$_backup_file"
    fi
}

# https://gist.github.com/hofmannsven/ff21749b0e6afc50da458bebbd9989c5
backup_default -g InitialKeyRepeat
defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)

backup_default -g KeyRepeat
defaults write -g KeyRepeat -int 1         # normal minimum is 2 (30 ms)

# Compact menu bar item spacing. Requires logout/login to take effect.
# Note: verify these keys still work on macOS 16+ — Apple has changed menu bar internals.
backup_default -g NSStatusItemSpacing
defaults write -g NSStatusItemSpacing -int 6

backup_default -g NSStatusItemSelectionPadding
defaults write -g NSStatusItemSelectionPadding -int 4

# Emacs tweaks
defaults write org.gnu.Emacs NSAppSleepDisabled -bool YES  # When in background do not sleep
defaults write org.gnu.Emacs ApplePressAndHoldEnabled -bool false  # Speed up the navigation keys
defaults write org.gnu.Emacs NSQuitAlwaysKeepsWindows -bool false  # Don't remember window position on quit
