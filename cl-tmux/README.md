# cl-tmux

tmux session manager for Claude Code.

## Install

Dependencies:

```bash
brew bundle --file=~/.dotfiles/cl-tmux/Brewfile
```

Add to `~/.tmux.conf`:

```tmux
run-shell ~/.dotfiles/cl-tmux/cl.tmux
```

Add to `~/.claude/settings.json`:

```json
"hooks": {
  "Notification":     [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker" }] }],
  "UserPromptSubmit": [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker" }] }],
  "Stop":             [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker" }] }]
}
```
