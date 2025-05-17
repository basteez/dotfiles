#!/bin/bash

if [ "$1" = "up" ]; then
    brightnessctl set +5%
elif [ "$1" = "down" ]; then
    brightnessctl set 5%-
fi

level=$(brightnessctl get)
max=$(brightnessctl max)
percent=$((level * 100 / max))

notify-send -u low -i display-brightness "🌞 Brightness: $percent%"

