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
import XMonad.Actions.WindowGo

import Data.Ratio
import Data.Maybe
import Data.List

import System.IO

shortcuts = spawnShortcuts myApps

myApps = ["&firefox", "k&calc", "&dolphin", "konsole -e &vim", "&opera", 
          "c&hromium-browser", "/opt/&netbeans-6.8/bin/netbeans", "&krusader",
          "&psi", "&amarok", "oo&writer", "k&3b", "&gimp", "&inkscape",
          "Virtual&Box"]

myWorkspaces = ["term", "web", "dev", "file", "doc", "misc", "chat", "vm", "float"]


-- TODO: make pretty, add gimp
positioning = composeAll
    [ className     =? "Firefox"        --> (doShift $ getWorkspace "web")
    , className     =? "Opera"          --> (doShift $ getWorkspace "web")
    , className     =? "Krusader"       --> (doShift $ getWorkspace "file")
    , className     =? "psi"            --> (doShift $ getWorkspace "chat")
    , className     =? "Amarok"         --> (doShift $ getWorkspace "misc")
    , className     =? "VirtualBox"     --> (doShift $ getWorkspace "vm")
    , className     =? "Chromium-browser" --> (doShift $ getWorkspace "web")
    , className     =? "Gimp"           --> (doShift $ getWorkspace "float")
    , className     =? "Gimp"           --> doFloat
    , className     =? "Kcalc"          --> doFloat
    , className     =? "Kmix"           --> doFloat
    , fmap ("VLC" `isInfixOf`) title    --> (doShift $ getWorkspace "float")
    , fmap ("VLC" `isInfixOf`) title    --> doFloat
    , fmap ("NetBeans" `isInfixOf`) title --> (doShift $ getWorkspace "dev")
    ]

myLayouts =   onWorkspace (getWorkspace "float")    simpleFloat
            $ onWorkspace (getWorkspace "dev")      full
            $ onWorkspace (getWorkspace "web")      full
            $ onWorkspace (getWorkspace "chat")     imLayout
            $ def
    where
        tabs    = (layoutHints $ avoidStruts $ tabbed shrinkText defaultTheme)
        def     = (avoidStruts $ tabs ||| layoutHook defaultConfig )
        imLayout = avoidStruts $ withIM (1 % 6) (Role "psimain") def
        full    = avoidStruts Full


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

marker m l = _marker m l [] where
    _marker m (x:xs) acc | m == x       = ([head xs], acc ++ xs)
                         | otherwise    = _marker m xs $ acc ++ [x]

spawnShortcuts l = map (_spawn . marker '&') l where
    _spawn (m, app) = ("M-x " ++ m, spawn app)

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


