@~/proj/dotfiles/agents/AGENTS.md

# Claude Code only

- Some paths are denied in settings (`~/.ssh`, `~/.gnupg`, `~/.zshrc`, which
  resolves into the dotfiles repo). When a tool call is denied, give Joe a
  `! <command>` to run in the prompt instead of finding another route.
- MCP tool calls ask for approval via a `PreToolUse` hook. Use MCP only when Joe
  asks for something that needs it.
