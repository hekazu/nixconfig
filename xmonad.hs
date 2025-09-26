import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Loggers

-- Ewhm compatibility
import XMonad.Hooks.EwmhDesktops

-- Xmobar compatibility
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- managehook utils
import XMonad.Hooks.ManageHelpers

-- startuphook utils
import XMonad.Util.SpawnOnce (spawnOnce)

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/nixconfig/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myconfig

myconfig = def
    {  terminal = "alacritty"
    ,  startupHook = myStartupHook
    ,  manageHook = myManageHook
    ,  focusedBorderColor = "#32CD32"
    }
  `additionalKeysP`
    [ ("M-f", spawn "librewolf")
    , ("M4-l", spawn "xscreensaver-command -lock")
    , ("M-q", restart "xmonad" True )
    , ("M-e", spawn "thunderbird")
    , ("<Print>", spawn "flameshot gui")
    ]

myXmobarPP :: PP
myXmobarPP = def

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "trayer --edge bottom --align right --SetDockType true \
            \--SetPartialStrut true --expand true --width 10 \
            \--transparent true --tint 0x5f5f5f --height 18"
  spawnOnce "feh --bg-fill --no-fehbg ~/wallpapers/nix.png"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "cbatticon -n -x \"shutdown -h now\""
  spawnOnce "nm-applet --sm-disable"
  spawnOnce "flameshot"

myManageHook :: ManageHook
myManageHook = composeAll
  [ isDialog --> doFloat
  , isInProperty "_NET_WM_WINDOW_TYPE"
                 "_NET_WM_WINDOW_TYPE_NOTIFICATION" --> doFloat
--  , isNotification --> doFloat | replaces above in xmonad-contrib-0.18.1
  ]
