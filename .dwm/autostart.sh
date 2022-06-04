#!/bin/sh

xrandr --output eDP --mode 2880x1800 --rate 60.00

~/.fehbg
sxhkd &
picom --experimental-backends &
nm-applet &
blueberry-tray &
emacs --daemon
greenclip daemon &
dwmblocks &
dropbox &
fcitx5 --replace -d
