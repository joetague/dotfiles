# ~/.zshenv
# Read by every zsh (login or not, interactive or not), so keep it tiny and
# free of subprocesses. Don't set PATH here: /etc/zprofile runs path_helper
# afterwards and reorders it. PATH and other env live in ~/.zprofile.

# Agent mode: explicit opt-in, or env vars the agent CLIs set themselves
# (Claude Code: CLAUDECODE, OpenCode: OPENCODE and AGENT, Codex: CODEX_THREAD_ID).
# Set here rather than in ~/.zshrc because Claude Code's Bash tool is a
# non-interactive login shell and its snapshot only replays PATH.
if [[ -n "$DISABLE_SHELL_TOOLING" || -n "$AGENT" || -n "$CLAUDECODE" \
      || -n "$OPENCODE" || -n "$CODEX_THREAD_ID" ]]; then
  export DISABLE_SHELL_TOOLING=1
  # Never block on an interactive pager or editor
  export PAGER=cat GIT_PAGER=cat MANPAGER=cat
  export GIT_EDITOR=true EDITOR=true
fi
