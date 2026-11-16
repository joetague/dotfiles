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
# Only .spacemacs is deployed. Private layers and snippets are left in this repo
# and resolved in place by `dotspacemacs-configuration-layer-path' and
# `auto-completion-private-snippets-directory' in the dotfile, so nothing is
# symlinked into ~/.emacs.d. Everything else in the package is repo metadata.
stow_package "$HOME" "spacemacs" \
  --ignore '^(private|plans|\.claude)$' \
  --ignore '^CLAUDE\.md$'
stow_package "$HOME" "ssh"
stow_package "$HOME" "gnupg"
stow_package "$HOME" "claude"

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

# Packages are installed separately so stowing stays quick and safe to re-run.
# There's no stored home/work profile: run the shared Brewfile plus the one for
# this machine.
cat <<EOF

Stowed. Install or update packages with:
  brew bundle --file=$repo_root/Brewfile
  brew bundle --file=$repo_root/Brewfile.home   # home laptop
  brew bundle --file=$repo_root/Brewfile.work   # work laptop

Untracked per-machine files (create as needed):
  ~/.zprofile.local       environment, also seen by coding agents
  ~/.zshrc.local          interactive aliases and functions
  ~/.gitconfig.local      git overrides, e.g. the work identity
  ~/.ssh/config.d/        per-machine SSH hosts
  ~/.claude/statusline.local   path to an extra status line script (work budget)
EOF
