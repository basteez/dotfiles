#!/bin/bash

SOURCE="@DEFAULT_AUDIO_SOURCE@"

toggle_mute() {
    wpctl set-mute $SOURCE toggle
}

is_muted() {
    wpctl get-volume $SOURCE | grep -q MUTED
    echo $?
}

# Esegui il toggle
toggle_mute

# Verifica stato dopo toggle
muted=$(is_muted)

if [ "$muted" -eq 0 ]; then
    notify-send -u low -i microphone-sensitivity-muted "Microfono off"
else
    notify-send -u low -i microphone-sensitivity-high "Microfono on"
fi
