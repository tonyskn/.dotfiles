 Config {
       font = "xft:Mensch:size=9:bold:antialias=true"
     , bgColor = "black"
     , fgColor = "grey"
     , position = Top
     , commands = [ Run CommandReader "~/.xmonad/pymodoro/pymodoro.py" "pomodoro"
                 , Run Weather "LFPO" ["-t", "<tempC>°"] 50
                 , Run Cpu ["-L", "3", "-H", "50", "-l", "green", "-n", "lightblue", "-h", "red"] 10
                 , Run Memory ["-t", "Mem: <usedratio>%", "-L", "20", "-H", "50", "-l", "green", "-n", "lightblue", "-h", "red"] 10
                 , Run Uptime [] 10
                 , Run Date "%a %b %_d %H:%M" "date" 10
                 , Run StdinReader ]
     , template = " %StdinReader%}{ %pomodoro% ~ %cpu% ~ %memory% ~ %uptime% ~ <fc=#ee9a00>%date% %LFPO%</fc> " }

