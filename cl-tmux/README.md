# cl-tmux

A tmux session picker and bookmark manager for Claude Code and Codex. It can
resume, fork, search, rename, and send prompts to agent sessions.

Harnesses are opt-in. The picker only sees sessions from harnesses whose hooks
you configure.

## Install

Dependencies:

```bash
brew bundle --file=~/.dotfiles/cl-tmux/Brewfile
```

Add to `~/.tmux.conf`:

```tmux
run-shell ~/.dotfiles/cl-tmux/cl.tmux
```

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
