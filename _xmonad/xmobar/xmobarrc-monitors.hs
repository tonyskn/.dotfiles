Config {
     bgColor = "black"
   , fgColor = "grey"
   , position = Bottom
   , commands =[ Run Uptime [] 10
               , Run TopProc [] 10
               , Run CommandReader "~/.xmonad/xmobar/xmobarrc-monitors.sh" "extra"
               , Run Network "wlp3s0" ["-L","0","-H","70","-l", "green", "-n","lightblue","-h","red"] 10
               , Run Wireless "wlp3s0" [] 10
               , Run BatteryP ["BAT0", "BAT1"] ["-t", "Bat: <watts>|<left>%|<timeleft>"] 10
               , Run BatteryN ["BAT0"] ["-t", "Bat0: <watts>|<left>%"] 10 "battery0"
               , Run BatteryN ["BAT1"] ["-t", "Bat1: <watts>|<left>%"] 10 "battery1" ]
   , template = "%wlp3s0% %wlp3s0wi% ~ %top% ~ %extra%}{%battery0% ~ %battery1% ~ %battery% ~ %uptime% " }
