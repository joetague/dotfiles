# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Personal Spacemacs configuration for Joe Tague. Contains the `.spacemacs` dotfile and private layers that extend the upstream Spacemacs framework at `~/.emacs.d` (the `develop` branch of https://github.com/syl20bnr/spacemacs).

## Deployment

Uses **GNU Stow** via `scripts/stow.sh`:
- `.spacemacs` is symlinked to `~/.spacemacs`
- `private/*` contents are symlinked into `~/.emacs.d/private/` (the layer directories, snippets, templates, local packages)

The stow command ignores `.spacemacs` when targeting `~/.emacs.d` and ignores `private/` when targeting `$HOME`, so each file ends up in the right place.

To deploy: `~/proj/dotfiles/scripts/stow.sh`
To verify: `~/proj/dotfiles/scripts/stow-check.sh`

## Layer Structure

All private layers follow standard Spacemacs layer conventions (see `~/.emacs.d/doc/CONVENTIONS.org`). Each has `packages.el` (required), and optionally `config.el`, `funcs.el`, `keybindings.el`.

Personal functions use the `jpt/` prefix. Layer-internal functions use `<layer-name>--` prefix (e.g., `personal-org--ensure-babel-languages`).

### Active Layers

- **personal** - Utility functions (`jpt/*`): JWT decoding, UUID generation, line manipulation, true/false toggling, unix timestamp conversion, native compilation helper. Keybindings under `SPC o` (user-reserved prefix) organized as `SPC o t` (text), `SPC o u` (utilities), `SPC o p` (projectile).

- **personal-devtools** - Packages: `mise`. Configures tree-sitter language sources and Projectile search path (`~/proj/` depth 2).

- **personal-shell** - Packages: `ghostel`, `evil-ghostel`, `hungry-delete`, `window-purpose`. Adds Ghostel as a shell backend on top of the upstream `shell` layer (popup: `SPC a t s g`). Ghostel global-compile and Eshell visual-command integration are off by default (toggle via `personal-shell-enable-*` vars). Set `shell-default-shell` to `ghostel` to make standard shell bindings use it.

- **personal-lsp** - No owned packages; configures LSP via `with-eval-after-load`. Sets up `emacs-lsp-booster` advice for JSON parsing performance. Disables on-type formatting and indentation (defers to language-specific tools like CIDER). Java LSP: custom JDT URL, 4GB heap, autobuild disabled. Pyright: multi-root disabled.

- **personal-magit** - No owned packages. Git binary: `/opt/homebrew/bin/git`. Repo dirs: `~/.emacs.d` and `~/proj/` (depth 4). Forge: owned account `joetague`, topic list limit 100.

- **personal-org** - Packages: `org-pdftools`, `org-ql`, `org-super-agenda`. Org files in `~/org/`. Agenda files: `learning.org`, `life.org`. Custom agenda view `z` with super-agenda grouping. Journal: weekly, encrypted, starts Monday. Babel languages loaded lazily on first src block execution (dot, elisp, gptel, js, python, shell, sqlite). GPG encryption for `:crypt:` tagged entries.

- **personal-llm** - Packages: `agent-shell` (init currently stubbed). gptel active backend/model is per-machine (set in `dotspacemacs/user-config` or `~/.spacemacs.local`); the layer only registers Kagi as a backend via auth-source. System prompt directives loaded from `~/proj/llm-prompts/` when present. API keys via `pass` (password-store).

### Inactive Layers (defined but not in layer list)

- **dape** - Debug Adapter Protocol
- **personal-lang-clojure** - Clojure tooling (clj-deps-new, jet, clay)
- **work-org** - Work-specific org capture templates

## .spacemacs Dotfile

The dotfile uses `dotspacemacs/user-config` for most settings and `with-eval-after-load` for lazy configuration. Supports machine-local overrides via `~/.spacemacs.local` (loaded at end of user-config if it exists).

Key choices:
- Completion: vertico (compleseus layer) with nerd-icons
- Editing style: vim
- Shell: vterm with zsh
- Theme: fleetish-theme (additional package)
- Window style: undecorated-round

## Conventions When Editing

- Configuration that tweaks an existing Spacemacs-managed package belongs in the relevant `personal-*` layer's `config.el` using `with-eval-after-load`, not in `.spacemacs`
- New packages go in the appropriate layer's `packages.el` with a `<layer>/init-<package>` function using `use-package`
- Layers with no owned packages (personal-lsp, personal-magit) still use `packages.el` with an empty list; all config goes in `config.el`
- User keybindings go under `SPC o` prefix in `personal/keybindings.el`
- Secrets use `auth-source` (`~/.authinfo.gpg`) or `pass` (password-store), never hardcoded

# MCP Usage Policy

By default, begin all sessions with MCP tools considered disabled and unavailable.

Do not proactively use, suggest, or rely on MCP servers, MCP resources, or MCP tool calls.

Only enable or use MCP capabilities after the user explicitly requests MCP usage or asks for an action that clearly requires a specific MCP server.

When MCP is requested:

1. Confirm which MCP server or capability is needed if it is not obvious.
2. Enable and use only the minimum MCP functionality required for the task.
3. Prefer non-MCP solutions when they adequately satisfy the request.
4. Do not keep MCP enabled beyond the scope of the requested task unless the user asks to continue using it.

Assume MCP is disabled at the start of every new session.
