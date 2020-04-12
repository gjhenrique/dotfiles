#!/bin/sh

RESPONSE=$((pgrep -a openvpn$ | head -n 1 | awk '{print $NF }' | cut -d '.' -f 1 && echo down) | head -n 1)

if [ "$RESPONSE" == "2" ]; then
    echo "%{F#82E0AA}Xing: Up%{F-}"
elif [ "$RESPONSE" != "--management-query-passwords" ]; then
    echo "%{F#f00}Xing: $RESPONSE%{F-}"
fi
