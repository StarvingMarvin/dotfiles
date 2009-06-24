
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

shortcuts = 
    [("M-x f", spawn "firefox")]

positioning = composeAll
    [className =? "Firefox"     --> doShift "2"]

main = do
    xmproc <- spawnPipe "xmobar /home/luka/.xmonad/xmobarrc"
    
    xmonad $ defaultConfig
        { terminal           = "urxvt"
        , manageHook         = positioning <+> manageDocks <+> manageHook defaultConfig
        , layoutHook         = avoidStruts  $  layoutHook defaultConfig
        , logHook            = dynamicLogWithPP $ xmobarPP
            { ppOutput       = hPutStrLn xmproc
            , ppTitle        = xmobarColor "green" "" . shorten 50
            }
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#778800" 
        } `additionalKeysP` shortcuts


