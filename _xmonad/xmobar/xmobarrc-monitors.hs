Config {
     bgColor = "black"
   , fgColor = "grey"
   , position = Bottom
   , commands =[ Run CpuFreq ["-t", "Freq:<cpu0>|<cpu1>|<cpu2>|<cpu3>", "-L", "0", "-H", "2", "-l", "green", "-n","lightblue", "-h", "red"] 50
               , Run Uptime [] 10
               , Run Wireless "wlp3s0" [] 10
               , Run TopProc [] 10
               , Run CommandReader "~/.xmonad/xmobar/xmobarrc-monitors.sh" "extra"
               , Run Network "wlp3s0" ["-L","0","-H","70","-l", "green", "-n","lightblue","-h","red"] 10
               , Run BatteryP ["BAT0"] ["-t", "<timeleft>", "-c", "charge_full"] 10 ]
   , template = "%wlp3s0wi%-%wlp3s0% ~ %top% ~ %cpufreq%}{%extra%|%battery% ~ %uptime% " }
