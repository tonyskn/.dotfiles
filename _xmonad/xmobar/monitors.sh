#!/bin/zsh
pkill -f xmobarrc-monitors > /dev/null
xmobar ~/.xmonad/xmobar/xmobarrc-monitors.hs
