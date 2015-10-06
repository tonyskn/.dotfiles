#!/bin/bash
declare FREQ='3s'

monitor() {
   local name=$1; shift
   eval $name=$( (eval "$@" > /dev/null && echo Y) || echo N )
}

while (true) do
   monitor BLUE_AUDIO  pactl list \| grep a2dp

   FAN_SPEED=$(grep -m1 level < /proc/acpi/ibm/fan | awk '{print $2}')

   echo "bluez: $BLUE_AUDIO ~ fans: $FAN_SPEED"

   sleep $FREQ
done

