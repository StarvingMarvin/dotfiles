import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.LayoutHints
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM

import Data.Ratio
import Data.Maybe
import Data.List

import System.IO

shortcuts = 
    [ ("M-x f", spawn "firefox")
    , ("M-x c", spawn "kcalc")
    , ("M-x d", spawn "dolphin")
    , ("M-x v", spawn "konsole -e vim")
    , ("M-x o", spawn "opera")
    , ("M-x g", spawn "chromium-browser")
    , ("M-x n", spawn "/opt/netbeans-6.8/bin/netbeans")
    , ("M-x k", spawn "krusader")
    , ("M-x p", spawn "psi")
    , ("M-x a", spawn "amarok")
    ]

positioning = composeAll
    [ className     =? "Firefox"        --> (doShift $ getWorkspace "web2")
    , className     =? "Opera"          --> (doShift $ getWorkspace "web")
    , className     =? "Krusader"       --> (doShift $ getWorkspace "file")
    , fmap ("NetBeans" `isInfixOf`) title --> (doShift $ getWorkspace "dev")
    , fmap ("VLC" `isInfixOf`) title    --> (doShift $ getWorkspace "float")
    , fmap ("VLC" `isInfixOf`) title    --> doFloat
    , appName       =? "kcalc"          --> (doShift $ getWorkspace "float")
    , appName       =? "kcalc"          --> doFloat
    , className     =? "Kmix"           --> doFloat
    , className     =? "Yakuake"        --> doIgnore
    , className     =? "stalonetray"    --> doIgnore
    , className     =? "Amarok"         --> (doShift $ getWorkspace "misc")
    , className     =? "VirtualBox"     --> (doShift $ getWorkspace "vm")
    ]

myLayouts =   onWorkspace (getWorkspace "term")     tabs
            $ onWorkspace (getWorkspace "float")    simpleFloat
            $ onWorkspace (getWorkspace "dev")      (dev ||| (avoidStruts Full))
            $ onWorkspace (getWorkspace "chat")     imLayout 
            $ def
    where
        def     = (avoidStruts $ layoutHook defaultConfig )
        tabs    = (layoutHints $ avoidStruts $ tabbed shrinkText defaultTheme)
        dev     = avoidStruts $ Mirror $ ResizableTall 1 (3 % 100) (2 % 3) []
        imLayout = avoidStruts $ withIM (1 % 6) (Role "psimain") def

myWorkspaces = ["term", "web", "dev", "file", "chat", "misc", "web2", "vm", "float"]

-- if there is less then 9 workspaces, they will be filled with ""
enumeratedWorkspaces = 
    zip (myWorkspaces ++ empty) [1..9]
    where
        empty = "" : empty

showWorkspace (name, id) = show id ++ ":" ++ name

showWorkspaces = 
    map showWorkspace enumeratedWorkspaces

-- returns string "id:name" if there is workspace with given name, "0:name" otherwise
getWorkspace name = showWorkspace ws
    where ws = (name, fromMaybe 0 $ lookup name enumeratedWorkspaces)


main = do
    xmproc <- spawnPipe "xmobar /home/luka/.xmonad/xmobarrc"

    xmonad $ defaultConfig
        { manageHook        = positioning <+> manageDocks <+> manageHook defaultConfig
        , startupHook       = setWMName "LG3D"
        , layoutHook        = myLayouts
        , logHook           = dynamicLogWithPP $ xmobarPP
            { ppOutput      = hPutStrLn xmproc
            , ppUrgent      = xmobarColor "green" "" . wrap "*" "*"
            , ppTitle       = xmobarColor "green" "" . shorten 100
            , ppLayout      = \x -> ""
            }
        , workspaces = showWorkspaces
        , terminal           = "konsole"
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#778800" 
        , modMask = mod4Mask
        } `additionalKeysP` shortcuts


