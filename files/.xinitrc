#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Disabling screensaver
xset -dpms &
#xset s noblank &

xrdb ~/.Xresources &

xrandr-settings &
(sleep 1 && set-keyboard) &

session=${2:-openbox}

case $session in
    openbox           ) exec openbox-session;;
    virtualbox        ) exec virtualbox;;
    xbmc              ) exec xbmc;;
    *                 ) exec $2;;
esac

