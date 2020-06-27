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
xrandr_settings &
(sleep 1 && set_keyboard) &
(sleep 2 && xmodmap ~/.Xmodmap) &

session=${2:-i3}

case $session in
    xfce ) exec startxfce4;;
    openbox ) exec openbox-session;;
    virtualbox ) exec virtualbox;;
    xbmc ) exec xbmc;;
    i3 ) exec i3;;
    * ) exec $2;;
esac
