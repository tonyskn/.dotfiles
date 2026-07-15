#!/bin/bash
WINDOW_ID=$(tmux display-message -t "$TMUX_PANE" -p '#{window_id}' 2>/dev/null)
TITLE=$(tmux display-message -t "$TMUX_PANE" -p '#W' 2>/dev/null)
TITLE="${TITLE#? }"
tmux rename-window -t "$WINDOW_ID" "${TITLE}"
