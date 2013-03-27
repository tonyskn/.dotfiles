#!/bin/bash
declare FREQ='3s'

monitor() {
   local name=$1; shift
   eval $name=$( (eval "$@" > /dev/null && echo Y) || echo N )
}

while (true) do
   monitor VPN         file /var/run/vpnc/pid 
   monitor NVIDIA      grep ON /proc/acpi/bbswitch
   monitor BLUE_AUDIO  pactl list \| grep a2dp

   FAN_SPEED=$(grep -m1 level < /proc/acpi/ibm/fan | awk '{print $2}')
   BATT_DRAIN=$(echo "scale=2; $(< /sys/devices/platform/smapi/BAT0/power_now)/1000" | bc)
   BATT_PERCENT=$(< /sys/devices/platform/smapi/BAT0/remaining_percent)

   echo "vpn: $VPN ~ nvidia: $NVIDIA ~ bluez: $BLUE_AUDIO ~ fans: $FAN_SPEED ~ Bat: $BATT_DRAIN|$BATT_PERCENT%"

   sleep $FREQ
done

