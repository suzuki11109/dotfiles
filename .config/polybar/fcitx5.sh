#!/bin/bash

# Use this for FCITX5
input=$(qdbus "org.fcitx.Fcitx5" "/controller" "org.fcitx.Fcitx.Controller1.CurrentInputMethod")

if [[ "$input" == "keyboard-us" ]]; then
    echo "en"
elif [[ "$input" == "keyboard-th" ]]; then
    echo "th"
elif [[ "$input" == "mozc" ]]; then
    echo "あ"
elif [[ "$input" == "" ]]; then
    echo " "
fi
