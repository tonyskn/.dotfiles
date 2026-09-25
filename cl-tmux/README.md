# cl-tmux

A tmux picker for tracked Claude Code and Codex sessions. It can resume, fork,
bookmark, search, and send prompts to sessions.

Hooks store live state on tmux panes and register sessions in a local index.
cl-tmux runs no background process or daemon. Removing a session drops its
index entry without deleting its transcript.

Harnesses are opt-in: configure their hooks to track new sessions. Use `ctrl-l`
for the bookmarked view (live sessions remain visible), and `ctrl-r` to search
tracked transcripts.

## Install

Dependencies:

```bash
brew bundle --file=~/.dotfiles/cl-tmux/Brewfile
```

Add to `~/.tmux.conf`:

```tmux
run-shell ~/.dotfiles/cl-tmux/cl.tmux
```

Open the picker with `prefix + u`. Window icons show `○` idle, `●` working,
and `◐` waiting.

To enable Claude Code, add to `~/.claude/settings.json`:

```json
"hooks": {
  "SessionStart":     [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker --harness claude" }] }],
  "Notification":     [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker --harness claude" }] }],
  "UserPromptSubmit": [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker --harness claude" }] }],
  "Stop":             [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker --harness claude" }] }]
}
```

To enable Codex, link the included hooks and approve them with `/hooks` on the
next Codex launch:

```bash
ln -s ~/.dotfiles/_codex/hooks.json ~/.codex/hooks.json
```

For Codex sessions using the shared daemon, set this in `~/.codex/config.toml`
so hooks can locate the tmux pane by session ID:

```toml
[tui]
terminal_title = ["app-name", "session-id"]
```
