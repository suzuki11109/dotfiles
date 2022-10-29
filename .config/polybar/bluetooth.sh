#!/usr/bin/env sh

if [ $(bluetoothctl show | grep "Powered: yes" | wc -c) -eq 0 ]
then
  echo "%{F#8087a2}"
else
  if [ $(echo info | bluetoothctl | grep 'Device' | wc -c) -eq 0 ]
  then
    echo "%{F#c6a0f6}"
  fi
  echo "%{F#c6a0f6}"
fi
