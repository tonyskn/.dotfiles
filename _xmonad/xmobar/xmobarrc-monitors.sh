#!/bin/zsh
ON='Y'
OFF='N'
FREQ='3s'

while (true) do
   VPN=$( (file /var/run/vpnc/pid > /dev/null && echo $ON) || echo $OFF )
   NVIDIA=$( (lspci -k | grep nvidia > /dev/null && echo $ON) || echo $OFF )
   BLUE_AUDIO=$( (pactl list | grep a2dp > /dev/null && echo $ON) || echo $OFF )

   FAN_SPEED=$(grep -m1 level < /proc/acpi/ibm/fan | awk '{print $2}')
   BATT_DRAIN=$(echo "scale=2; $(< /sys/devices/platform/smapi/BAT0/power_now)/1000" | bc)
   BATT_PERCENT=$(< /sys/devices/platform/smapi/BAT0/remaining_percent)

   echo "vpn: $VPN ~ nvidia: $NVIDIA ~ bluez: $BLUE_AUDIO ~ fans: $FAN_SPEED ~ Bat: $BATT_DRAIN|$BATT_PERCENT%"

   sleep $FREQ
done

