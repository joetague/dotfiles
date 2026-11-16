# Agent instructions (all projects)

Shared by Claude Code, Codex and OpenCode on both of Joe's Macs. Source:
`~/proj/dotfiles/agents/AGENTS.md`; project files (`AGENTS.md`, `CLAUDE.md`)
add to it.

## Shell

- zsh on macOS, in agent mode: `~/.zshenv` sees the agent and sets
  `DISABLE_SHELL_TOOLING=1`, so commands are stock (no aliases), `PAGER=cat`,
  and `EDITOR=true`. Give every command its input up front: `git commit -m`,
  `--yes`/`--no-edit` style flags, never `-i`/`-p` interactive modes.
- GNU coreutils, sed, grep and awk come before the macOS versions on `PATH`,
  so `sed -i`, `stat -c`, `date -d` take GNU syntax. `find` and `xargs` are
  BSD. For a BSD tool, call `/usr/bin/<tool>`.
- `bash` is macOS's 3.2: shell scripts avoid `mapfile`, associative arrays and
  `${var,,}`, or say they need zsh.
- Search with `rg` and `fd`. Delete with `trash <path>` (recoverable) instead
  of `rm -rf`.
- Language runtimes come from mise (`mise ls`), packages from Homebrew via the
  Brewfiles in `~/proj/dotfiles`. Ask before installing anything new.

## Git

- Treat `main`/`master` as protected everywhere: commit on a branch, push it,
  and open a pull request with `gh pr create`; Joe merges. Work repos enforce
  this on GitHub; personal repos don't, so it relies on you.
- Pushing uses Joe's 1Password SSH key and may show an approval prompt; wait
  for it rather than retrying.

## Secrets

- Secrets live in 1Password (`op`) or Emacs auth-source. Reference them; keep
  their values out of files, commits and command output.
- `~/.ssh` and `~/.gnupg` are Joe's to change: write the commands for him to
  run.
