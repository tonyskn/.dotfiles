# cl-tmux

A tmux session picker and bookmark manager for Claude Code. It can resume,
fork, search, rename, and send prompts to Claude sessions.

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
  "SessionStart":     [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker --harness claude" }] }],
  "Notification":     [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker --harness claude" }] }],
  "UserPromptSubmit": [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker --harness claude" }] }],
  "Stop":             [{ "hooks": [{ "type": "command", "command": "~/.dotfiles/cl-tmux/bin/tmux-marker --harness claude" }] }]
}
```
