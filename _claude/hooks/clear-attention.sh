#!/bin/bash
WINDOW_ID=$(tmux display-message -t "$TMUX_PANE" -p '#{window_id}' 2>/dev/null)
PANE_TITLE=$(tmux display-message -t "$TMUX_PANE" -p '#W' 2>/dev/null)
# Strip any prefix
CLEAN_TITLE="${PANE_TITLE#? }"
CLEAN_TITLE="${CLEAN_TITLE#- }"
tmux rename-window -t "$WINDOW_ID" "${CLEAN_TITLE}"
