# CLAUDE.md

Personal Spacemacs configuration: the `.spacemacs` dotfile plus private layers extending upstream Spacemacs at `~/.emacs.d` (the `develop` branch of https://github.com/syl20bnr/spacemacs). Repo-wide deployment and stow rules are in `../CLAUDE.md`.

## Deployment

Only `.spacemacs` is stowed (to `~/.spacemacs`); nothing is symlinked into `~/.emacs.d`. The dotfile points Spacemacs back at this repo:
- `dotspacemacs-configuration-layer-path` → `~/proj/dotfiles/spacemacs/private/`
- `auto-completion-private-snippets-directory` → `~/proj/dotfiles/spacemacs/private/snippets/`

`private/`, `plans/`, `.claude/`, and `CLAUDE.md` are excluded from stow via `--ignore` in `scripts/stow.sh`.

## Layers

Layers follow Spacemacs conventions (`~/.emacs.d/doc/CONVENTIONS.org`). Each layer's `packages.el` lists the packages it owns. Read it rather than relying on a summary here. Personal functions use the `jpt/` prefix; layer-internal functions use `<layer-name>--`.

Active (see `dotspacemacs-configuration-layers` in `.spacemacs`):

- **personal**: `jpt/*` utilities (JWT decode, UUIDs, line manipulation, true/false toggle, unix timestamps, native-comp helper). Keybindings under `SPC o` (`SPC o t` text, `SPC o u` utilities, `SPC o p` projectile).
- **personal-devtools**: `mise`, `treesit-auto`, tree-sitter language sources, Projectile search path (`~/proj/`, depth 2).
- **personal-lsp**: no owned packages. `emacs-lsp-booster` advice; on-type formatting and indentation off (language tools like CIDER own that); Java: pinned JDT download URL, 4GB heap, autobuild off; Pyright multi-root off.
- **personal-magit**: no owned packages. Git at `/opt/homebrew/bin/git`; repo dirs `~/.emacs.d` and `~/proj/` (depth 4); Forge owned account `joetague`.
- **personal-org**: org files in `~/org/`, agenda files `learning.org` and `life.org`, custom agenda view `z` (org-super-agenda), weekly encrypted journal starting Monday, `:crypt:` entries GPG-encrypted. Babel languages load lazily on first src-block execution.
- **personal-llm**: agent-shell and agent-shell-hq (`SPC $ a`), gptel and gptel-agent (`SPC $ g`), gptel-quick on Embark `?`, ob-gptel for org-babel. Backend is LM Studio: `personal-llm/refresh-lmstudio-models` fetches the model list from `personal-llm-lmstudio-base-url` (default `http://localhost:1234`). gptel pauses `window-purpose` while it runs (gptel issue #237).

Inactive (defined, not in the layer list):

- **dape**: Debug Adapter Protocol.
- **personal-lang-clojure**: Clojure tooling stubs (clj-deps-new, jet, clay).
- **personal-shell**: extras on the upstream `shell` layer, which owns `ghostel`/`evil-ghostel` since upstream `a10be2cb1`, so this layer must not define `init-` functions or `:toggle`s for them. Adds a magit `s` alias, a ghostel org-babel language, `SPC p '`/`SPC p $` project shells, and `terminal-here` (`SPC "`).
- **work-org**: work org-capture templates.

## .spacemacs

Environment comes from `~/.spacemacs.env`, pinned in `dotspacemacs/user-env`. Without the pin the path depends on how Emacs was launched (Dock vs terminal). Regenerate it with `SPC f e E` after changing `zsh/.zprofile`. `user-env` keeps `CC`/`LIBRARY_PATH` (emacs-plus owns those) and per-shell state (`PWD`, `__MISE_*`, starship and 1Password markers) out of the file; add new per-shell variables to that list.

Most settings live in `dotspacemacs/user-config`, configured lazily with `with-eval-after-load`. `~/.spacemacs.local`, if present, is loaded at the end of user-config for machine-local overrides.

Key choices: vertico (compleseus) with nerd-icons; vim editing style; shell is ghostel with zsh via the upstream `shell` layer (`SPC a t s g`), with vterm support disabled; fleetish-theme; undecorated-round frames.

## Conventions When Editing

- Tweaks to a Spacemacs-managed package go in the relevant `personal-*` layer's `config.el` inside `with-eval-after-load`, keeping `.spacemacs` lean.
- New packages go in the right layer's `packages.el` with a `<layer>/init-<package>` function using `use-package`.
- Layers that own no packages still keep `packages.el` with an empty list; their config lives in `config.el`.
- User keybindings go under `SPC o` in `personal/keybindings.el`.
- Secrets come from `auth-source` (`~/.authinfo.gpg`), never hardcoded.
