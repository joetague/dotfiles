#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
errors=0

check_link() {
  local target=$1
  local expected=$2

  if [[ ! -L "$target" ]]; then
    printf 'missing symlink: %s\n' "$target"
    errors=$((errors + 1))
    return
  fi

  local actual
  actual="$(readlink "$target")"
  if [[ "$actual" != "$expected" ]]; then
    printf 'mismatched symlink: %s -> %s (expected %s)\n' "$target" "$actual" "$expected"
    errors=$((errors + 1))
  fi
}

check_link "$HOME/.zshrc" "$repo_root/zsh/.zshrc"
check_link "$HOME/.zprofile" "$repo_root/zsh/.zprofile"
check_link "$HOME/.config/git/config" "$repo_root/.config/git/config"
check_link "$HOME/.config/git/ignore" "$repo_root/.config/git/ignore"
check_link "$HOME/.config/starship.toml" "$repo_root/.config/starship.toml"
check_link "$HOME/.spacemacs" "$repo_root/spacemacs/.spacemacs"
check_link "$HOME/.emacs.d/private" "$repo_root/spacemacs/private"
check_link "$HOME/.ssh/config" "$repo_root/ssh/.ssh/config"
check_link "$HOME/.gnupg/gpg.conf" "$repo_root/gnupg/.gnupg/gpg.conf"
check_link "$HOME/.gnupg/gpg-agent.conf" "$repo_root/gnupg/.gnupg/gpg-agent.conf"
check_link "$HOME/.gnupg/scdaemon.conf" "$repo_root/gnupg/.gnupg/scdaemon.conf"

if [[ $errors -gt 0 ]]; then
  exit 1
fi
