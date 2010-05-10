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

import Data.Ratio
import Data.Maybe

import System.IO

shortcuts = 
    [ ("M-x f", spawn "firefox")
    , ("M-x c", spawn "kcalc")
    , ("M-x d", spawn "dolphin")
    , ("M-x v", spawn "urxvt -e vim")
    ]

positioning = composeAll
    [ className =? "Firefox"     --> (doShift $ getWorkspace "web")
    , appName   =? "kcalc"       --> (doShift $ getWorkspace "float")
    , appName   =? "kcalc"       --> doFloat
    ]

myLayouts =   onWorkspace (getWorkspace "term")     tabs 
            $ onWorkspace (getWorkspace "float")    simpleFloat
            $ onWorkspace (getWorkspace "dev")      (dev ||| (avoidStruts Full))
            $ (avoidStruts $ layoutHook defaultConfig )
    where
        tabs    = (layoutHints $ avoidStruts $ tabbed shrinkText defaultTheme)
        dev     = avoidStruts $ Mirror $ ResizableTall 1 (3 % 100) (2 % 3) []

myWorkspaces = ["term", "web", "dev", "file", "doc", "float"]

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
    xmproc <- spawnPipe "xmobar /home/quantum/.xmonad/xmobarrc"

    xmonad $ defaultConfig
        { manageHook         = positioning <+> manageDocks <+> manageHook defaultConfig
	, startupHook	     = setWMName "LG3D"
        , layoutHook         = myLayouts
        , logHook            = dynamicLogWithPP $ xmobarPP
            { ppOutput       = hPutStrLn xmproc
            , ppTitle        = xmobarColor "green" "" . shorten 60
            }
        , workspaces = showWorkspaces
        , terminal           = "urxvt"
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#778800" 
        , modMask = mod4Mask
        } `additionalKeysP` shortcuts


