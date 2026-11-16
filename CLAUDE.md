# CLAUDE.md

Joe Tague's macOS dotfiles, deployed with GNU Stow. Each top-level directory below is a stow package; everything else is repo-only.

## Deployment

The repo copy is the source of truth: `~/.zshrc`, `~/.spacemacs`, `~/.config/git`, etc. are symlinks into this repo, so edit here and never replace a deployed link with a plain file.

- Deploy: `scripts/stow.sh` (also chmods `~/.ssh` and `~/.gnupg`)
- Verify: `scripts/stow-check.sh`. When you add a deployed file, add a `check_link` line for it.
- Lint shell: `./test.sh` (shellcheck over every shell file)

| Package | Target | Contents |
|---|---|---|
| `.config` | `~/.config` | git, starship, ghostty, opencode, clojure, helm, ideavim, emacs-plus, spacemacs env |
| `zsh` | `~` | `.zshenv` (agent detection), `.zprofile` (environment, PATH), `.zshrc` (interactive). Per-machine additions go in untracked `~/.zprofile.local` / `~/.zshrc.local`. |
| `spacemacs` | `~` | only `.spacemacs`; see `spacemacs/CLAUDE.md` |
| `ssh`, `gnupg` | `~` | allowlisted safe config only |
| `claude` | `~` | `~/.claude/statusline-command.sh`, shared by home and work. Machine differences live in untracked files: `~/.claude/settings.json` and `~/.claude/statusline.local` (path to the work budget script). |

Not stowed: `Brewfile` (shared) plus `Brewfile.home` / `Brewfile.work` (per machine), `scripts/macos_tweaks.sh` (backs up each `defaults` key to `$XDG_DATA_HOME/macos-tweaks` before changing it), `scripts/org-capture.sh`, and `.dockerfunc` / `.myfuncs` (not sourced by the zsh config).

## Gotchas

- Stow folds whole directories under `~/.config` into single symlinks, so tools that write runtime state next to their config write it into this repo. Gitignore that state (see `.gitignore`) rather than stowing around it.
- `ssh/.ssh/.gitignore` and `gnupg/.gnupg/.gitignore` ignore everything except named files. Add a new config file to that allowlist; keys, `known_hosts`, and keyrings stay out of the repo.
- zsh startup order is `.zshenv` → `/etc/zprofile` (`path_helper` reorders PATH) → `.zprofile` → `.zshrc`. Set PATH and environment in `.zprofile`; keep `.zshenv` tiny.
- Agent shells: `.zshenv` sets `DISABLE_SHELL_TOOLING=1` when it detects Claude Code, OpenCode, or Codex. This makes pagers and editors no-ops, and `.zshrc` skips aliases (`cat`→`bat`, `cp -i`, …) and interactive tooling (fzf, starship, zoxide, 1Password). Gate any new interactive-only setup on `[[ -z "$DISABLE_SHELL_TOOLING" ]]`.
- Secrets come from `auth-source` (`~/.authinfo.gpg`) or 1Password; keep them out of tracked files.
