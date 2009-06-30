
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Util.Themes
import Data.Maybe
--import XMonad.Prompt
--import XMonad.Prompt.Shell
--import XMonad.Prompt.Ssh
--import XMonad.Prompt.Theme
--import XMonad.Prompt.Window
--import XMonad.Prompt.XMonad
--import XMonad.Prompt.Layout
--import XMonad.Prompt.Man
--import XMonad.Prompt.AppendFile
--import XMonad.Prompt.Input
import System.IO

shortcuts = 
    [ ("M-x f", spawn "firefox")
--	, ("M-s", shellPrompt defaultXPConfig)
	]

positioning = composeAll
    [ className =? "Firefox"     --> (doShift $ getWorkspace "web")
    , className =? "KCalc"       --> (doShift $ getWorkspace "float")
    , className =? "KCalc"       --> doFloat
   -- , className =? "CGoban"     --> doFloat
    ]

-- tabbed shrinkText (theme smallClean)
myLayouts = onWorkspace (getWorkspace "term")  (avoidStruts $ simpleTabbed) 
            $ onWorkspace (getWorkspace "float")  simpleFloat
            $ (avoidStruts $ layoutHook defaultConfig )

myWorkspaces = ["term", "web", "dev", "file", "doc", "float"]

enumeratedWorkspaces = 
    zip (myWorkspaces ++ empty) [1..9]
    where
        empty = "" : empty

showWorkspace (name, id) = show id ++ ":" ++ name

showWorkspaces = 
    map showWorkspace enumeratedWorkspaces

getWorkspace name = showWorkspace ws
    where ws = (name, fromMaybe 0 $ lookup name enumeratedWorkspaces)

--promptConfig = defaultXPConfig
--    { font = "-*-terminus-normal-*-*-*-12-*-*-*-*-*-*-*"
--	, bgColor = "black"
--	, fgColor = "#FFFFFF"
--	, fgHLight = "#3377AA"
--	, bgHLight = "#000000"
--	, borderColor = "#444444"
--	, promptBorderWidth = 1
--	, position = Bottom
--	, height = 16
--	, historySize = 50
--	}

main = do
    xmproc <- spawnPipe "xmobar /home/luka/.xmonad/xmobarrc"
    
    xmonad $ defaultConfig
        { manageHook         = positioning <+> manageDocks <+> manageHook defaultConfig
-- avoidStruts  $  layoutHook defaultConfig
        , layoutHook         = avoidStruts $ layoutHook defaultConfig
        , logHook            = dynamicLogWithPP $ xmobarPP
            { ppOutput       = hPutStrLn xmproc
            , ppTitle        = xmobarColor "green" "" . shorten 60
            }
        , workspaces = showWorkspaces
        , terminal           = "urxvt"
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#778800" 
        } `additionalKeysP` shortcuts


