#!/bin/bash
set -euo pipefail

DOTFILES="$HOME/.dotfiles"

link() {
  local src="$DOTFILES/$1"
  local dst="$HOME/$2"
  if [ -L "$dst" ]; then
    echo "  ok: $2"
    return
  fi
  if [ -e "$dst" ]; then
    echo "skip: $dst exists and is not a symlink"
    return
  fi
  mkdir -p "$(dirname "$dst")"
  ln -s "$src" "$dst"
  echo "link: $dst -> $src"
}

# Home directory dotfiles
link _zshrc        .zshrc
link _gitconfig    .gitconfig
link _git_aliases  .git_aliases
link _tmux.conf    .tmux.conf
link _vimrc        .vimrc
link _tigrc        .tigrc
link _jq           .jq
link _bin          .bin

# XDG config
link _config/starship.toml  .config/starship.toml
link _config/nvim/init.lua  .config/nvim/init.lua

# Claude Code
link _claude/settings.json  .claude/settings.json
link _claude/hooks          .claude/hooks
