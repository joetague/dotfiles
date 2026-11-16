#!/usr/bin/env bash
# Claude Code status line, shared between machines.
#
# Line 1: dir (bold cyan) git-branch (bold purple) | model | worktree | agent
#         | ctx% | 5h% · 7d% (claude.ai subscription limits, only when Claude
#         Code sends `rate_limits`, i.e. on a Pro/Max login).
# Line 2: output of a machine-local external script, if configured (work
#         budget). The path goes on the first line of ~/.claude/statusline.local,
#         which is untracked.
#
# Single jq call; git branch read from .git/HEAD (no git subprocess).
# Bash 3.2 compatible (macOS /bin/bash).

CYAN=$'\033[1;36m'
PURPLE=$'\033[1;35m'
GREEN=$'\033[32m'
GREY=$'\033[90m'
YELLOW=$'\033[33m'
RED=$'\033[31m'
RESET=$'\033[0m'

EXTERNAL_CONF="$HOME/.claude/statusline.local"
EXTERNAL_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/claude-statusline/external"
EXTERNAL_TTL=60     # seconds between external script runs
EXTERNAL_TIMEOUT=3  # seconds before the external script is abandoned

input=$(cat)
[ -n "$input" ] || exit 0

IFS=$'\x1f' read -r cwd model worktree agent pct h5 h5_reset d7 d7_reset < <(
  printf '%s' "$input" | jq -r '
    def pct: if . == null then "" else floor | tostring end;
    def str: if . == null then "" else tostring end;
    [ (.workspace.current_dir // .cwd // ""),
      (.model.display_name // ""),
      (.workspace.git_worktree // ""),
      (.agent.name // ""),
      (.context_window.used_percentage | pct),
      (.rate_limits.five_hour.used_percentage | pct),
      (.rate_limits.five_hour.resets_at | str),
      (.rate_limits.seven_day.used_percentage | pct),
      (.rate_limits.seven_day.resets_at | str)
    ] | join("\u001f")' 2>/dev/null
) || exit 0

# Directory, abbreviated like Starship (~ for home)
case "$cwd" in
  "$HOME"*) dir="~${cwd#"$HOME"}" ;;
  *)        dir="$cwd" ;;
esac
[ -n "$dir" ] || exit 0

# Git branch: walk up to find .git, read HEAD
branch=""
d="$cwd"
while [ -n "$d" ] && [ "$d" != "/" ]; do
  if [ -d "$d/.git" ]; then
    head="$d/.git/HEAD"
  elif [ -f "$d/.git" ]; then
    gd=$(sed -n 's/^gitdir: //p' "$d/.git" 2>/dev/null)
    case "$gd" in /*) ;; *) gd="$d/$gd" ;; esac
    head="$gd/HEAD"
  else
    d="${d%/*}"
    continue
  fi
  if [ -r "$head" ]; then
    ref=$(<"$head")
    case "$ref" in
      "ref: refs/heads/"*) branch="${ref#ref: refs/heads/}" ;;
      *) branch="${ref:0:7}" ;;
    esac
  fi
  break
done

# color_for PCT WARN CRIT
color_for() {
  if   [ "$1" -ge "$3" ]; then printf '%s' "$RED"
  elif [ "$1" -ge "$2" ]; then printf '%s' "$YELLOW"
  else                         printf '%s' "$GREEN"
  fi
}

# fmt_epoch EPOCH FORMAT (BSD date, falling back to GNU)
fmt_epoch() {
  date -r "$1" "+$2" 2>/dev/null || date -d "@$1" "+$2" 2>/dev/null
}

# limit_segment LABEL PCT RESETS_AT RESET_FORMAT
# Shows the reset time once usage is critical, so you know how long the wait is.
limit_segment() {
  local color reset=""
  color=$(color_for "$2" 70 90)
  if [ "$2" -ge 90 ] && [ -n "$3" ]; then
    reset=" ↻$(fmt_epoch "$3" "$4")"
  fi
  printf '%s%s %s%%%s%s' "$color" "$1" "$2" "$reset" "$RESET"
}

line="${CYAN}${dir}${RESET}"
[ -n "$branch" ] && line="${line} ${PURPLE} ${branch}${RESET}"
[ -n "$model" ] && line="${line} ${GREY}|${RESET} ${model}"
[ -n "$worktree" ] && line="${line} ${GREY}|${RESET} ${worktree}"
[ -n "$agent" ] && line="${line} ${GREY}|${RESET} ${agent}"
[ -n "$pct" ] && line="${line} ${GREY}|${RESET} $(color_for "$pct" 50 80)ctx ${pct}%${RESET}"

# Subscription limits (home). Absent behind an API gateway such as Portkey.
limits=""
[ -n "$h5" ] && limits=$(limit_segment 5h "$h5" "$h5_reset" '%H:%M')
if [ -n "$d7" ]; then
  [ -n "$limits" ] && limits="${limits} ${GREY}·${RESET} "
  limits="${limits}$(limit_segment 7d "$d7" "$d7_reset" '%a %H:%M')"
fi
[ -n "$limits" ] && line="${line} ${GREY}|${RESET} ${limits}"

# External script (work budget via Portkey).
# STUB: its output is printed verbatim as line 2. It calls Portkey itself, so
# runs are cached for EXTERNAL_TTL seconds and killed after EXTERNAL_TIMEOUT.
# Failures and timeouts are cached too (as empty), so a broken script costs at
# most one timeout per TTL. TODO: parse its % to colour it / drive alerts.
external_line() {
  local cmd mtime now tmp
  [ -r "$EXTERNAL_CONF" ] || return 0
  IFS= read -r cmd < "$EXTERNAL_CONF" || [ -n "$cmd" ] || return 0
  [ "${cmd#\~/}" != "$cmd" ] && cmd="$HOME/${cmd#\~/}"
  [ -x "$cmd" ] || return 0

  now=$(date +%s)
  if [ -f "$EXTERNAL_CACHE" ]; then
    # perl, not stat: `stat -f` means different things to BSD and GNU stat,
    # and Homebrew coreutils may put GNU stat first on PATH.
    mtime=$(perl -e 'print +(stat shift)[9]' "$EXTERNAL_CACHE" 2>/dev/null)
    if [ -n "$mtime" ] && [ $((now - mtime)) -lt "$EXTERNAL_TTL" ]; then
      cat "$EXTERNAL_CACHE"
      return 0
    fi
  fi

  mkdir -p "${EXTERNAL_CACHE%/*}" || return 0
  tmp="$EXTERNAL_CACHE.$$"
  # macOS has no coreutils `timeout`; perl's alarm survives exec.
  printf '%s' "$input" \
    | perl -e 'alarm shift; exec @ARGV or exit 127' "$EXTERNAL_TIMEOUT" "$cmd" \
      >"$tmp" 2>/dev/null || : >"$tmp"
  mv -f "$tmp" "$EXTERNAL_CACHE"
  cat "$EXTERNAL_CACHE"
}

external=$(external_line)

printf '%s' "$line"
[ -n "$external" ] && printf '\n%s' "$external"
exit 0
