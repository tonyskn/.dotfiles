 Config {
       bgColor = "black"
     , fgColor = "grey"
     , position = Top
     , commands = [ Run Weather "EDDT" ["-t", "<tempC>°"] 50
                 , Run Brightness ["-t", "Bri: <percent>%"] 1
                 , Run Cpu ["-L", "3", "-H", "50", "-l", "green", "-n", "lightblue", "-h", "red"] 10
                 , Run Memory ["-t", "Mem: <usedratio>%", "-L", "20", "-H", "50", "-l", "green", "-n", "lightblue", "-h", "red"] 10
                 , Run Uptime [] 10
                 , Run Date "%a %b %_d %H:%M" "date" 10
                 , Run StdinReader ]
     , template = " %StdinReader%}{ %cpu% ~ %memory% ~ %uptime% ~ %bright% ~ <fc=#ee9a00>%date% %EDDT%</fc> " }

