import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.UrgencyHook
-- import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Util.EZConfig
import XMonad.Prompt
import XMonad.Prompt.Shell
import System.IO

-- Define workspaces as (workspaceId, [className]) tuples where
-- [className] contains Xsession classNames of the apps bound to workspaceId
--
-- XMonad FAQ below explains how that works
-- [http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program]
workspaces' = [ ("1:main", ["Google-chrome", "Thunderbird"])
              , ("2:term", [])
              , ("3:idea", ["java-lang-Thread"])
              , ("4:chat", ["Gajim.py"])
              , ("5:whatever", [])
              ]

layoutHook' = onWorkspace "3:idea" nobordersLayout
            $ onWorkspace "4:chat" chatLayout
            $ Mirror tiled1 ||| nobordersLayout
    where
        tiled1 = spacing 5 $ Tall nmaster1 delta ratio
        nmaster1 = 1
        ratio = 1/2
        delta = 3/100
        nobordersLayout = noBorders $ Full
        gridLayout = spacing 8 $ Grid
        chatLayout = withIM (20/100) (Role "roster") gridLayout

manageHook' = foldl1 (<+>) $ do
    (id, xCNames) <- workspaces'
    xCName <- xCNames
    return (className =? xCName --> doShift id)

logHook' xmobar = do
    dynamicLogWithPP xmobarPP'
    takeTopFocus -- fixes glitches in JAVA GUI apps
--     ewmhDesktopsLogHook
    where
        xmobarPP' = xmobarPP
            { ppOutput = hPutStrLn xmobar
            , ppTitle = xmobarColor "green" "" . shorten 50  -- sends current window title to xmobar
            , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
            , ppLayout = const "" -- disables layout display on xmobar
            }

-- launch gnome-terminal without menu-bar
-- in Ubuntu, you may need to run this is order to make it work
-- apt-get remove appmenu-gtk3 appmenu-gtk appmenu-qt
terminal' = "gnome-terminal --hide-menubar"

-- On Ubuntu, you need to deactivate nautilus triggers:
-- Remove /usr/share/applications/nautilus*.desktop
startupHook' = mapM_ spawnOnce $
    [ "gnome-settings-daemon"
    , "thunar --daemon"
    , "~/apps/davmail/davmail.sh"
    , "~/.dropbox-dist/dropboxd"
    , "feh --bg-scale ~/.dotfiles/world-map-wallpaper.png"
    ]

xpc = defaultXPConfig { bgColor  = "black"
                      , fgColor  = "yellow"
                      , font = "xft:Bitstream Vera Sans Mono:size=9:bold:antialias=true"
                      , promptBorderWidth = 0
                      , position = Bottom
                      , height   = 16
                      , historySize = 256 }

main = do
   xmobar <- spawnPipe "/usr/bin/xmobar ~/.xmonad/.xmobarrc"
   xmonad $ withUrgencyHook NoUrgencyHook $ azertyConfig
        { workspaces = map fst workspaces'
        , manageHook = manageDocks <+> manageHook' <+> manageHook azertyConfig
--         , handleEventHook = fullscreenEventHook
        , startupHook = startupHook'
        , logHook = logHook' xmobar
        , layoutHook = avoidStruts $ layoutHook'
        , normalBorderColor  = "#586e75" -- solarized base01
        , focusedBorderColor = "#cb4b16" -- solarized orange
        , borderWidth = 2
        , terminal = terminal'
        , modMask = mod4Mask
        , focusFollowsMouse = False
        } `additionalKeysP`
            [ ("M-p", shellPrompt xpc)
            , ("M-f", spawn "thunar")
            , ("M-b", sendMessage ToggleStruts)
            , ("M-<Backspace>", focusUrgent)
            , ("M-n", spawn "touch ~/.pomodoro_session")
            , ("M-S-n", spawn "rm ~/.pomodoro_session")
            ]
          `additionalMouseBindings`
            -- disable floating windows on mouse left-click
            [ ((mod4Mask, button1), const $ return ()) ]
