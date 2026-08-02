#!/usr/bin/env bash
# TPM plugin entry point for cl-tmux.

PLUGIN_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN="$PLUGIN_DIR/bin"

# Inject status icon before #W in both window formats.
ICON_PREFIX='#{?@cl_icon,#{@cl_icon} ,}'
for opt in window-status-format window-status-current-format; do
  fmt=$(tmux show-option -gv "$opt")
  if [[ "$fmt" != *"@cl_icon"* ]]; then
    tmux set-option -g "$opt" "${fmt/\#W/${ICON_PREFIX}#W}"
  fi
done

# Session picker popup.
tmux bind-key u display-popup -E -w 80% -h 60% -T ' Claude Sessions ' "$BIN/cl"
