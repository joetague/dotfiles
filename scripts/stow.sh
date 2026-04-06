#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

stow_package() {
  local target=$1
  local package=$2
  shift 2
  stow -d "$repo_root" -t "$target" "$package" "$@"
}

stow_package "$HOME/.config" ".config"
stow_package "$HOME" "zsh"
stow_package "$HOME/.emacs.d" "spacemacs" --ignore '^\\.spacemacs$'
stow_package "$HOME" "spacemacs" --ignore '^(private|snippets)$' --ignore '^(private|snippets)/'
stow_package "$HOME" "ssh"
stow_package "$HOME" "gnupg"

# Lock down permissions for SSH/GnuPG configs.
chmod 700 "$HOME/.ssh" "$HOME/.gnupg"
for file in "$HOME/.ssh/config" \
  "$HOME/.gnupg/gpg.conf" \
  "$HOME/.gnupg/gpg-agent.conf" \
  "$HOME/.gnupg/scdaemon.conf"; do
  if [[ -e "$file" ]]; then
    chmod 600 "$file"
  fi
done
