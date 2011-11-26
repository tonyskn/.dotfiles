import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Util.EZConfig
import System.IO

-- Define amount and names of workspaces
myWorkspaces = ["1:main","2:vim","3:idea","4:chat","5:whatever"]

myLayout = onWorkspace "3:idea" nobordersLayout
            $ onWorkspace "4:chat" chatLayout
            $ Mirror tiled1 ||| nobordersLayout  
    where  
        tiled1 = spacing 5 $ Tall nmaster1 delta ratio  
        nmaster1 = 1  
        ratio = 1/2  
        delta = 3/100  
        nobordersLayout = noBorders $ Full  
        gridLayout = spacing 8 $ Grid
        chatLayout = withIM (20/100) (Role "contact_list") gridLayout

-- Assigns applications to workspaces
-- XMonad FAQ below explains how that works
-- [http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program]
myManageHook = composeAll (
    [ className =? "Google-chrome" --> doShift "1:main"
    , className =? "Thunderbird" --> doShift "1:main"
    -- , className =? "Gnome-terminal" --> doShift "2:term"
    , className =? "java-lang-Thread" --> doShift "3:idea"
    , className =? "Empathy" --> doShift "4:chat" ])

startup :: X()
startup = do
      spawnOnce "gnome-settings-daemon"
      spawnOnce "thunar --daemon"
      spawnOnce "davmail"
      spawnOnce "~/.dropbox-dist/dropboxd"
      spawnOnce "feh --bg-scale ~/.dotfiles/world-map-wallpaper.png" 

main = do
       xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/.xmobarrc"
       xmonad $ azertyConfig
        { manageHook = manageDocks <+> myManageHook <+> manageHook azertyConfig
        , startupHook = startup
        , logHook = dynamicLogWithPP xmobarPP  
            { ppOutput = hPutStrLn xmproc  
            , ppTitle = xmobarColor "green" "" . shorten 50  -- sends current window title to xmobar  
            , ppLayout = const ""                            -- to disable the layout info on xmobar
            } >> takeTopFocus >> setWMName "LG3D"            -- fixes Java GUI issues
        , layoutHook = avoidStruts $ myLayout
        , workspaces = myWorkspaces
        , normalBorderColor  = "#586e75" -- solarized base01
        , focusedBorderColor = "#cb4b16" -- solarized orange
        , borderWidth = 2
        , modMask = mod4Mask
        , focusFollowsMouse = False
        } `additionalKeys`  
        [ ((mod4Mask .|. shiftMask, xK_Return), spawn "gnome-terminal --hide-menubar" ) -- launch terminal without menu
        , ((mod4Mask, xK_p), spawn "exe=`dmenu_run -b -nb black -nf yellow -sf yellow` && eval \"exec $exe\"") -- spawn dmenu
        , ((mod4Mask, xK_f), spawn "thunar")
        , ((mod4Mask, xK_b), sendMessage ToggleStruts)
        ] `additionalMouseBindings`
        [ ((mod4Mask, button1),\_ -> return()) ]
