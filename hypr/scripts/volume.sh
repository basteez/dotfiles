#!/bin/bash

SINK="@DEFAULT_AUDIO_SINK@"

get_volume() {
    wpctl get-volume $SINK | awk '{print int($2 * 100)}'
}

is_muted() {
    wpctl get-volume $SINK | grep -q MUTED
    echo $?
}

case "$1" in
    up)
        wpctl set-volume $SINK 5%+
        ;;
    down)
        wpctl set-volume $SINK 5%-
        ;;
    mute)
        wpctl set-mute $SINK toggle
        ;;
    *)
        echo "Usage: $0 [up|down|mute]"
        exit 1
        ;;
esac

volume=$(get_volume)
muted=$(is_muted)

if [ "$muted" -eq 0 ]; then
    notify-send -u low -i audio-volume-muted "Muted"
else
    notify-send -u low -i audio-volume-high "Volume: ${volume}%"
fi
