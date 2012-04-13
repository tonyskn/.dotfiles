{-# OPTIONS -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import Control.Monad
import System.Environment (getEnvironment)

import ToggleSpawn

-- XMonad execution mode
data Mode = DEFAULT | LAPTOP
-- if `XMONAD_LAPTOP_MODE` env variable exists, we're on my laptop ;)
mode :: X Mode
mode = io $ guessFrom =<< getEnvironment
    where guessFrom = return . maybe DEFAULT (const LAPTOP) . lookup "XMONAD_LAPTOP_MODE"

-- Define workspaces as (workspaceId, [className]) tuples where
-- [className] contains the X WM_CLASS propertes of the windows
-- bound to workspaceId.
workspaces' = [ ("1:main", ["Google-chrome", "Hotot"])
              , ("2:term", [])
              , ("3:ide" , ["jetbrains-idea"])
              , ("4:chat", ["Gajim", "Gajim.py"])
              , ("5:misc", ["Spotify", "Vlc"])
              , ("6:misc", ["Skype", "VirtualBox"])
              , ("7:scratch", ["Firefox"]) ]

layoutHook' = onWorkspace "3:ide" nobordersLayout
            $ onWorkspace "4:chat" chatLayout
            $ tiled1 ||| nobordersLayout
    where tiled1 = spacing 5 $ Tall nmaster1 delta ratio
          nmaster1 = 1
          ratio = 17/24
          delta = 3/100
          nobordersLayout = noBorders Full
          chatLayout = withIM (20/100) (Role "roster") (spacing 8 Grid)

-- [ubuntu] apt-get remove appmenu-gtk3 appmenu-gtk appmenu-qt
terminal' = "gnome-terminal --hide-menubar"

startupHook' = mapM_ spawnOnce . startupItems
    where startupItems LAPTOP = [ unclutter, "nm-applet", "dropboxd" ]
          startupItems DEFAULT = [ unclutter, "gnome-settings-daemon", "~/.dropbox-dist/dropboxd", background ]
          unclutter = "unclutter -idle 1 -reset"
          background = "feh --bg-scale ~/.dotfiles/world-map-wallpaper.png"

toggleMonitorBar DEFAULT = return ()
toggleMonitorBar LAPTOP = do
    toggleSpawn "xmobar ~/.xmonad/xmobar/xmobarrc-monitors.hs"
    replicateM_ 4 (sendMessage $ ToggleStrut D)

xpConfig' = defaultXPConfig { bgColor  = "black", fgColor  = "yellow"
                      , font = "xft:Mensch:size=10:bold:antialias=true"
                      , position = Top, promptBorderWidth = 0
                      , height = 25, historySize = 256 }

xmobar' = statusBar xmobar pp toggleStrutsKey
    where
        xmobar = "xmobar ~/.xmonad/xmobar/xmobarrc.hs"
        pp = xmobarPP
           { ppTitle = xmobarColor "green" ""
            , ppLayout = const ""
            , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip }
        toggleStrutsKey = const (mod4Mask, xK_b)

main = xmonad <=< xmobar' $ withUrgencyHook NoUrgencyHook $ azertyConfig
        { workspaces = map fst workspaces'
        , startupHook = startupHook' =<< mode
        , logHook = takeTopFocus -- fixes glitches in Java GUI apps
        , normalBorderColor  = "#586e75" -- solarized base01
        , focusedBorderColor = "#cb4b16" -- solarized orange
        , borderWidth = 2
        , terminal = terminal'
        , modMask = mod4Mask
        , focusFollowsMouse = False
        , layoutHook = layoutHook'
        , handleEventHook = docksEventHook
        , manageHook = manageHook' }
        `additionalKeysP`
            [ ("M-p"  , shellPrompt xpConfig')
            , ("M-S-q", spawn "pkill 'gnome-session|xmonad'")
            , ("M-f"  , spawn "nautilus --no-desktop")
            , ("M-S-b", toggleMonitorBar =<< mode)
            , ("M-<Left>", moveTo Prev NonEmptyWS)
            , ("M-<Right>", moveTo Next NonEmptyWS)
            , ("M-<Backspace>", focusUrgent)
            , ("M-n"  , spawn "touch ~/.pomodoro_session")
            , ("M-S-n", spawn "rm ~/.pomodoro_session")
            , ("M-S-,", spawn "gnome-control-center")
            , ("M-s"  , spawn "gnome-screenshot -i") ]
        `additionalMouseBindings`
            -- disable floating windows on mouse left-click
            [ ((mod4Mask, button1), const $ return ()) ]
    where manageHook' = foldl1 (<+>) $ do
            (id, xCNames) <- workspaces'
            xCName <- xCNames
            return (className =? xCName --> doShiftAndGo id)
          doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
