#!/usr/bin/env bash
# Claude Code status line
# Mirrors a Starship-style prompt: user@host dir | model | context%

GREY="\033[90m"
YELLOW="\033[33m"
RED="\033[31m"
RESET="\033[0m"

input=$(cat)

worktree=$(echo "$input" | jq -r '.workspace.git_worktree // ""')
agent=$(echo "$input" | jq -r '.agent.name // ""')

token_display=$(echo "$input" | jq -r '
  ((.context_window.current_usage.input_tokens // 0)
   + (.context_window.current_usage.cache_creation_input_tokens // 0)
   + (.context_window.current_usage.cache_read_input_tokens // 0)) as $t |
  (.context_window.context_window_size // 0) as $max |
  if $t > 0 then
    (if $t >= 1000 then ($t / 1000 | tostring | split(".")[0]) + "." + (($t % 1000) / 100 | floor | tostring) + "k"
     else $t | tostring end) as $fmt |
    (if $max > 0 then " (" + (($t * 100 / $max) | floor | tostring) + "%)" else "" end) as $pct |
    $fmt + $pct + "\t" + ($t | tostring)
  else "" end')

line=""

if [ -n "$worktree" ]; then
    line="${worktree}"
fi

if [ -n "$agent" ]; then
    line="${line:+${line} | }${agent}"
fi

if [ -n "$token_display" ]; then
    fmt=$(echo "$token_display" | cut -f1)
    raw=$(echo "$token_display" | cut -f2)
    if   [ "$raw" -ge 90000 ]; then color="$RED"
    elif [ "$raw" -ge 50000 ]; then color="$YELLOW"
    else                             color="$GREY"
    fi
    line="${line:+${line} | }${color}${fmt}${RESET}"
fi

printf "%b" "$line"
