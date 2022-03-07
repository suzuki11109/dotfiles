import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen, nextWS, prevWS)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.Minimize
import XMonad.Actions.FloatKeys
import qualified XMonad.Actions.Search as S

import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.Minimize
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.StatusBar.PP (filterOutWsPP)

import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Minimize
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- import XMonad.Config.Prime (refresh)

import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad

import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Prompt.Input
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Man
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh
import Control.Arrow ((&&&), first)

import Colors.DoomOne

tabFontName :: String
tabFontName = "xft:Noto Sans:size=10,Noto Sans Thai:size=10"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "kitty"

--myTerminal2 :: String
--myTerminal2 = "wezterm"

myBrowser :: String
myBrowser = "brave "

myBrowser2 :: String
myBrowser2 = "qutebrowser "

myFileManager :: String
myFileManager = "thunar"

--myFileManager2 :: String
--myFileManager2 = "pcmanfm"

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "

myBorderWidth :: Dimension
myBorderWidth = 3

myNormColor :: String
myNormColor = color01

myFocusColor :: String
myFocusColor = color06

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer"
  --spawn "killall ibus-daemon"
  --spawnOnce "feh -z --bg-fill --no-fehbg /home/rd/wallpapers/pics/TokyoNight"
  --spawnOnce "lxsession"
  spawnOnce "picom"
  spawnOnce "nm-applet --sm-disable"
  spawnOnce "blueman-applet"
  --spawnOnce "sleep 3 && volumeicon"
  --spawnOnce "/usr/bin/emacs --daemon"
  spawnOnce "dunst"
  --spawnOnce "flameshot"
--  spawnOnce "/home/rd/.cargo/bin/eww daemon"
--  spawn "sleep 3 && /home/rd/.config/eww/launch_eww"
  -- spawnOnce "sxhkd"

  --spawn "sleep 2 && ibus-daemon"
  spawn ("sleep 2 && trayer --edge top --distance 4 --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 " ++ colorTrayer ++ " --height 38")

  -- spawnOnce "nitrogen --restore &"
  setWMName "XMonad"

-- myColorizer :: Window -> Bool -> X (String, String)
-- myColorizer = colorRangeFromClassName
--                   (0x1a,0x1b,0x29) -- lowest inactive bg
--                   (0x1a,0x1b,0x29) -- highest inactive bg
--                   (0xc7,0x92,0xea) -- active bg
--                   (0xc0,0xa7,0x9a) -- inactive fg
--                   (0x1a,0x1b,0x29) -- active fg
-- 
-- -- gridSelect menu layout
-- myGridConfig :: p -> GSConfig Window
-- myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
--     { gs_cellheight   = 40
--     , gs_cellwidth    = 200
--     , gs_cellpadding  = 6
--     , gs_originFractX = 0.5
--     , gs_originFractY = 0.5
--     , gs_font         = myFont
--     }
-- 
-- spawnSelected' :: [(String, String)] -> X ()
-- spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
--     where conf = def
--                    { gs_cellheight   = 40
--                    , gs_cellwidth    = 200
--                    , gs_cellpadding  = 6
--                    , gs_originFractX = 0.5
--                    , gs_originFractY = 0.5
--                    , gs_font         = myFont
--                    }
-- 
-- 
-- 
-- myAppGrid = [ ("Kitty", "alacritty")
--                  , ("Brave", "brave")
--                  , ("Emacs", "emacsclient -c -a emacs")
--                  , ("Firefox", "firefox")
--                  , ("Geany", "geany")
--                  , ("Qutebrowser", "qutebrowser")
--                  , ("Gimp", "gimp")
--                  , ("Kdenlive", "kdenlive")
--                  , ("LibreOffice Impress", "loimpress")
--                  , ("LibreOffice Writer", "lowriter")
--                  , ("Libreoffice", "loffice")
--                  , ("Libreoffice Calc", "localc")
--                  , ("Libreoffice Base", "lobase")
--                  , ("Libreoffice Math", "lomath")
--                  , ("Libreoffice Draw", "lodraw")
--                  , ("OBS", "obs")
--                  , ("Teams", "teams")
--                  , ("PCManFM", "pcmanfm")
--                  , ("Alacritty", "alacritty")
--                  , ("htop", myTerminal ++ " -e htop")
--                  , ("Neovim", myTerminal ++ " -e nvim")
--                  , ("Kakoune", myTerminal ++ " -e kak")
--                  , ("gtop", myTerminal ++ " -e gtop")
--                  , ("Change Theme", myTerminal ++ " -e ./.local/bin/changetheme")
--                  , ("Network", myTerminal ++ " -e nmtui")
--                  ]

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True


tall     = renamed [Replace "Tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 5
           $ minimize
           $ ResizableTall 1 (3/100) (1/2) []
tallR    = renamed [Replace "TallR"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 5
           $ reflectHoriz
           $ minimize
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "Magnify"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ minimize
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "Monocle"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ minimize
           $ limitWindows 20 Full
floats   = renamed [Replace "Floats"]
           $ smartBorders
           $ minimize
           $ limitWindows 20 simplestFloat
--grid     = renamed [Replace "Grid"]
--           $ smartBorders
--           $ windowNavigation
--           $ addTabs shrinkText myTabTheme
--           $ subLayout [] (smartBorders Simplest)
--           $ limitWindows 12
--           $ mySpacing 8
--           $ mkToggle (single MIRROR)
--           $ minimize
--           $ Grid (16/10)
threeCol = renamed [Replace "ThreeColumn"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 9
           $ mySpacing 2
           $ minimize
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "Tabs"]
           $ minimize
           $ tabbed shrinkText myTabTheme

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = tabFontName
                 , activeColor         = color05
                 , inactiveColor       = "#404552"
                 , activeBorderColor   = color05
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = color01
                 , inactiveTextColor   = color16
                 , decoHeight          = 36
                 }

-- Theme for showWName whereis prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:JetBrainsMono:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#282c34"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| withBorder myBorderWidth tallR
                                 ||| noBorders monocle
                                 ||| Main.magnify
				 ||| noBorders Full
                                 ||| noBorders tabs
                                 -- ||| withBorder myBorderWidth grid
                                 ||| withBorder myBorderWidth threeCol



myWorkspaces = ["web", "dev", "tool", "chat", "mail", "mus", "doc", "file", "misc"]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                -- , NS "obs" spawnOBS findOBS manageOBS
                , NS "calculator" spawnCalc findCalc manageCalc
                , NS "fm" spawnFM findFM manageFM
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnOBS  = "obs"
    findOBS   = className =? "obs"
    manageOBS = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w
    spawnFM    = "thunar"
    findFM     = className =? "Thunar"
    manageFM   = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "confirm"                   --> doFloat
     , className =? "file_progress"             --> doFloat
     , className =? "dialog"                    --> doFloat
     , className =? "download"                  --> doFloat
     , className =? "error"                     --> doFloat
     , className =? "Gimp"                      --> doFloat
     , className =? "notification"              --> doFloat
     , className =? "pinentry-gtk-2"            --> doFloat
     , className =? "splash"                    --> doFloat
     , className =? "toolbar"                   --> doFloat
     , className =? "Pavucontrol"               --> doCenterFloat
     , title     =? "Bluetooth"                 --> doFloat
     , title     =? "emacs-run-launcher"        --> doCenterFloat
     , className =? "Godot_Engine"              --> doFloat
     , className =? "Yad"                       --> doCenterFloat
     , className =? "Microsoft Teams - Preview" --> doShift (myWorkspaces !! 4)
     , className =? "Microsoft Teams - Preview" --> hasBorder True
     , className =? "kdenlive"                  --> doShift (myWorkspaces !! 7)
     , className =? "Gimp"                      --> doShift (myWorkspaces !! 7)
     , className =? "zoom"                      --> doShift (myWorkspaces !! 6)
     , isFullscreen -->  doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads

--myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
--myXPKeymap = M.fromList $
--  map (first $ (,) controlMask)   -- control + <key>
--  [ (xK_z, killBefore)            -- kill line backwards
--  , (xK_k, killAfter)             -- kill line fowards
--  , (xK_a, startOfLine)           -- move to the beginning of the line
--  , (xK_e, endOfLine)             -- move to the end of the line
--  , (xK_m, deleteString Next)     -- delete a character foward
--  , (xK_b, moveCursor Prev)       -- move cursor forward
--  , (xK_f, moveCursor Next)       -- move cursor backward
--  , (xK_BackSpace, killWord Prev) -- kill the previous word
--  , (xK_y, pasteString)           -- paste a string
--  , (xK_g, quit)                  -- quit out of prompt
--  , (xK_bracketleft, quit)
--  ] ++
--  map (first $ (,) mod4Mask)       -- meta key + <key>
--  [ (xK_BackSpace, killWord Prev) -- kill the prev word
--  , (xK_f, moveWord Next)         -- move a word forward
--  , (xK_b, moveWord Prev)         -- move a word backward
--  , (xK_d, killWord Next)         -- kill the next word
--  , (xK_n, moveHistory W.focusUp')
--  , (xK_p, moveHistory W.focusDown')
--  ]
--  ++
--  map (first $ (,) 0) -- <key>
--  [ (xK_Return, setSuccess True >> setDone True)
--  , (xK_KP_Enter, setSuccess True >> setDone True)
--  , (xK_BackSpace, deleteString Prev)
--  , (xK_Delete, deleteString Next)
--  , (xK_Left, moveCursor Prev)
--  , (xK_Right, moveCursor Next)
--  , (xK_Home, startOfLine)
--  , (xK_End, endOfLine)
--  , (xK_Down, moveHistory W.focusUp')
--  , (xK_Up, moveHistory W.focusDown')
--  , (xK_Escape, quit)
--  ]
--
--myXPConfig = def
--      { font                  = "xft:JetBrainsMono NF:size=14"
--        , bgColor             = colorBack
--        , fgColor             = colorFore
--        , bgHLight            = color06
--        , fgHLight            = "#000000"
--        , borderColor         = colorBack
--        , promptBorderWidth   = 1
--        , promptKeymap        = myXPKeymap
--        , position            = Top
--       -- , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
--        , height              = 28
--        , historySize         = 256
--        , historyFilter       = id
--        , defaultText         = []
--        , autoComplete        = Nothing    -- set Just 100000 for .1 sec
--        , showCompletionOnTab = False
--        , searchPredicate     = S.isPrefixOf
--        , alwaysHighlight     = True
--        , maxComplRows        = Nothing       -- set to Just 5 for 5 rows
--        }
--
--calcPrompt :: XPConfig -> String -> X () 
--calcPrompt c ans =
--    inputPrompt c (trim ans) ?+ \input -> 
--        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c 
--    where
--        trim  = f . f
--            where f = reverse . dropWhile isSpace
--
--
--whereisPrompt :: XPConfig -> String -> X ()
--whereisPrompt c ans =
--  inputPrompt c (trim ans) ?+ \input ->
--        liftIO(runProcessWithInput "whereis" [input] "") >>= whereisPrompt c
--      where f = reverse . dropWhile isSpace
--
--shPrompt :: XPConfig -> String -> X ()
--shPrompt c ans =
--  inputPrompt c (trim ans) ?+ \input ->
--        liftIO(runProcessWithInput "alacritty -e" [input] "") >>= shPrompt c
--      where f = reverse . dropWhile isSpace

-- START_KEYS
myKeys :: [(String, X ())]
myKeys =
    -- KB_GROUP WM
  [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart")             -- Restart XMonad
  -- , ("M-M1-r", spawn "xrefresh")
  , ("M-S-c", io exitSuccess)                       -- Exit XMonad
  -- , ("M-S-/", spawn "/home/rd/.config/xmonad/xmonad-keys") -- View keybindings
  -- , ("M-M1-t", spawn "/home/rd/.local/bin/changetheme")    -- Change theme (script in .local/bin/changetheme)
  -- , ("M-/", spawn "sleep 3 && /home/rd/.config/eww/launch_eww")
  -- , ("M-M1-/", spawn "feh -z --bg-fill /home/rd/wallpapers/pics/TokyoNight")
  -- , ("M-M1-S-/", spawn "feh --bg-fill -z -r /home/rd/wallpapers/pics")

  -- KB_GROUP Exit
  -- , ("M-x M-x", spawn "arcolinux-logout")
  -- , ("M-x l", spawn ".local/bin/lock")

  -- KB_GROUP Prompts
  --, ("M-r <Space>", shellPrompt myXPConfig)
  --, ("M-r M-r", shellPrompt myXPConfig)
  --, ("M-r x", xmonadPrompt myXPConfig)
  --, ("M-r m", manPrompt myXPConfig)
  --, ("M-r s", sshPrompt myXPConfig)
  --, ("M-r p p", passPrompt myXPConfig)
  --, ("M-r p g", passGeneratePrompt myXPConfig)
  --, ("M-r p r", passRemovePrompt myXPConfig)
  --, ("M-r c", calcPrompt myXPConfig "qalc")
  --, ("M-r w", whereisPrompt myXPConfig "whereis")
  --, ("M-r h", shPrompt myXPConfig "exec")
  --, ("M-r o", workspacePrompt myXPConfig (windows . W.greedyView))
  
  -- KB_GROUP Floating Windows
  , ("M-M1-<Left>",  withFocused (keysResizeWindow (10,0) (1,1)))
  , ("M-M1-<Right>", withFocused (keysResizeWindow (-10,0) (1,1)))
  , ("M-M1-<Up>",    withFocused (keysResizeWindow (0,10) (1,1)))
  , ("M-M1-<Down>",  withFocused (keysResizeWindow (0,-10) (1,1)))
  , ("M-M1-S-<Left>",   withFocused (keysMoveWindow (-10,0)))
  , ("M-M1-S-<Right>",  withFocused (keysMoveWindow (10,0)))
  , ("M-M1-S-<Up>",     withFocused (keysMoveWindow (0,-10)))
  , ("M-M1-S-<Down>",   withFocused (keysMoveWindow (0,10)))

  -- KB_GROUP Run Scripts
  --, ("M-p b", spawn "./run-scripts/dm-bookmarks")    -- Run script to go to web bookmark
  --, ("M-p m", spawn "./run-scripts/dm-commands")     -- Run script to run common commands
  --, ("M-p e", spawn "./run-scripts/dm-confedit")  -- Run script to edit config
  --, ("M-p c", spawn "./run-scripts/dm-currencies")
  --, ("M-p h", spawn "./run-scripts/dm-history")
  --, ("M-p i", spawn "./run-scripts/dm-kill")
  --, ("M-p l", spawn "./run-scripts/dm-lg")
  --, ("M-p o", spawn "./run-scripts/dm-maim")
  --, ("M-p d", spawn "./run-scripts/dm-man")
  --, ("M-p n", spawn "./run-scripts/dm-note")
  --, ("M-p r", spawn "./run-scripts/dm-rec")
  --, ("M-p t", spawn "./run-scripts/dm-translate")
  --, ("M-p g", spawn "./run-scripts/dm-wall")
  --, ("M-p w", spawn "./run-scripts/dm-weather")
  --, ("M-p s", spawn "./run-scripts/dm-websearch")
  --, ("M-p k", spawn "rofi -show keys")
  --, ("M-p f", spawn "rofi -show filebrowser")

  -- KB_GROUP Run Launcher
  -- , ("M-S-<Return>", spawn "rofi -show drun")              -- Run Launcher (Only GUI)
  --, ("M-S-d", shellPrompt myXPConfig)
  --, ("M-e <Return>", spawn "emacsclient -e '(emacs-run-launcher)'")
  --, ("M-M1-<Return>", spawn "rofi -show run -display-run 'Run'")       -- Run Launcher (traditional)
  --, ("M1-S-<Return>", spawn "rofi -show window -display-run") -- Show open windows
  , ("M-p", spawn "dmenu_run -fn 'JetBrainsMono-9'")

  -- KB_GROUP Application launchers
  , ("M-<Return>", spawn (myTerminal))                        -- Spawn terminal
  --, ("M-d", spawn (myTerminal2))                               -- Spawn alt terminal
  --, ("M-b w", spawn (myTerminal ++ " -e nmtui"))                  -- Spawn nmtui (for network management) in terminal
  --, ("M-b p", spawn "pavucontrol")
  --, ("M-b M-b", spawn (myBrowser))                         -- Spawn browser
  --, ("M-b S-b", spawn "nyxt")                         -- Spawn nyxt browser
  --, ("M-b b", spawn (myBrowser2))                        -- Spawn alt browser
  --, ("M-b n", spawn (myBrowser ++ " --incognito"))       -- Spawn private window in browser (only in chromium)
  --, ("M-b t", spawn ("brave --tor"))                     -- Spawn brave with tor window
  --, ("M-b z", spawn "./Applications/zulip")              -- Spawn Zulip GUI
  --, ("M-b S-z", spawn (myTerminal ++ " -e zulip-term"))             -- Spawn Zulip TUI
  --, ("M-b M-f", spawn (myFileManager))                     -- Spawn alt file manager
  --, ("M-b f", spawn (myFileManager2))                    -- Spawn file manager

  -- KB_GROUP Panel
  --, ("M-M1-b p", spawn "./.config/xmonad/to-polybar.sh")
  --, ("M-M1-b x", spawn "./.config/xmonad/to-xmobar.sh")
  --, ("M-M1-b b", spawn "./.config/xmonad/kill-bars.sh")
  --, ("M-M1 b M1-b", spawn "./.config/xmonad/to-polybar.sh")
  --, ("M-M1 b S-b", spawn "./.config/xmonad/to-xmobar.sh")

  -- KB_GROUP Kill
  , ("M-q", kill1)    -- Kill selected window
  , ("M-S-a", killAll)  -- Kill all windows in workspace

  -- KB_GROUP Float/Tile
  , ("M-f", sendMessage (T.Toggle "Floats")) -- Toggle floating layout
  , ("M-t", withFocused $ windows . W.sink)  -- Tile selected floating window
  , ("M-S-t", sinkAll)                 -- Tile all floating windows

  -- KB_GROUP App Grid
  --, ("M-g g", spawnSelected' myAppGrid)                  -- Spawn app grid to launch app
  --, ("M-g t", goToSelected $ myGridConfig myColorizer)   -- Spawn app grid to switch to app
  --, ("M-g b", bringSelected $ myGridConfig myColorizer)  -- Spawn app grid to bring app to current workspace

  -- KB_GROUP Clients
  , ("M-m", windows W.focusMaster)                     -- Move focus to the master window
  , ("M-j", windows W.focusDown)                       -- Move focus to the next window
  , ("M-k", windows W.focusUp)                         -- Move focus to the prev window
  , ("M-S-m", windows W.swapMaster)              -- Swap the focused window and the master window
  , ("M-S-j", windows W.swapDown)                -- Swap focused window with next window
  , ("M-S-k", windows W.swapUp)                  -- Swap focused window with prev window
  , ("M-<Backspace>", promote)                   -- Moves focused window to master, others maintain order
  -- , ("M-M1-<Tab>", rotSlavesDown)                     -- Rotate all windows except master and keep focus in place
  -- , ("M-C-<Tab>", rotAllDown)                        -- Rotate all the windows in the current stack
  , ("M-S-<Up>", sendMessage (IncMasterN 1))        -- Increase # of clients master pane
  , ("M-S-<Down>", sendMessage (IncMasterN (-1)))     -- Decrease # of clients master pane
  , ("M-C-<Up>", increaseLimit)                     -- Increase # of windows
  , ("M-C-<Down>", decreaseLimit)                     -- Decrease # of windows
  , ("M-h", sendMessage Shrink)                        -- Shrink horiz window width
  , ("M-l", sendMessage Expand)                        -- Expand horiz window width
  , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
  , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width
  --, ("M-C-h", sendMessage $ pullGroup L)
  --, ("M-C-l", sendMessage $ pullGroup R)
  --, ("M-C-k", sendMessage $ pullGroup U)
  --, ("M-C-j", sendMessage $ pullGroup D)
  --, ("M-C-m", withFocused (sendMessage . MergeAll))
  --, ("M-C-u", withFocused (sendMessage . UnMerge))
  --, ("M-C-/", withFocused (sendMessage . UnMergeAll))
  --, ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
  --, ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab
  -- , ("M-n", withFocused minimizeWindow)
  -- , ("M-S-n", withLastMinimized maximizeWindowAndFocus)

  -- KB_GROUP Scratchpads
  , ("M-s t", namedScratchpadAction myScratchPads "terminal")
  , ("M-s f", namedScratchpadAction myScratchPads "fm")
  --, ("M-s o", namedScratchpadAction myScratchPads "obs")
  , ("M-s s", namedScratchpadAction myScratchPads "sysmon")
  , ("M-s c", namedScratchpadAction myScratchPads "calculator")

  -- KB_GROUP Window Spacing
  , ("C-M1-k", decWindowSpacing 4)         -- Decrease window spacing
  , ("C-M1-j", incWindowSpacing 4)         -- Increase window spacing

  -- KB_GROUP Layouts
  , ("M-<Space>", sendMessage NextLayout)                                     -- Go to next layout
  --, ("M1-S-<Tab>", sendMessage FirstLayout)                                     -- Go to next layout
  , ("M-S-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggle struts
  , ("M-C-f", sendMessage (MT.Toggle NBFULL))
  , ("M-S-b", sendMessage ToggleStruts)                                                                                          

   -- KB_GROUP Workspace
  --, ("M-<Tab>", nextWS) -- Go to next workspace
  --, ("M-S-<Tab>", prevWS) -- Go to previous workspace

  -- KB_GROUP Emacs
  --, ("M-e e", spawn (myEmacs))
  --, ("M-e M-e", spawn (myEmacs ++ ("--eval '(dashboard-refresh-buffer)'")))                     -- emacs dashboard
  --, ("M-e b", spawn (myEmacs ++ ("--eval '(ibuffer)'")))                                      -- list buffers
  --, ("M-e d", spawn (myEmacs ++ ("--eval '(dired nil)'")))                                    -- dired
  --, ("M-e i", spawn (myEmacs ++ ("--eval '(erc)'")))                                          -- erc irc client
  --, ("M-e n", spawn (myEmacs ++ ("--eval '(elfeed)'")))                                       -- elfeed rss
  --, ("M-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))                                       -- eshell
  --, ("M-e v", spawn (myEmacs ++ ("--eval '(+vterm/here nil)'")))                              -- vterm if on Doom Emacs
  --, ("M-e w", spawn (myEmacs ++ ("--eval '(doom/window-maximize-buffer(eww \"gnu.org\"))'"))) -- eww browser


  -- KB_GROUP Media
  --, ("<XF86AudioPlay>", spawn "playerctl play-pause")
  --, ("<XF86AudioPrev>", spawn "playerctl previous")
  --, ("<XF86AudioNext>", spawn "playerctl next")

  , ("<XF86AudioRaiseVolume>"   , spawn "pamixer -i 3") 
  , ("<XF86AudioLowerVolume>"   , spawn "pamixer -d 3")
  , ("<XF86AudioMute>"          , spawn "pamixer --toggle-mute") 
  , ("<XF86MonBrightnessUp>"    , spawn "brightnessctl s 15+")
  , ("<XF86MonBrightnessDown>"  , spawn "brightnessctl s 15-")

  -- KB_GROUP Screenshots
  --, ("M-S-f f f", spawn "flameshot full -p ~/ScreenShots")
  --, ("M-S-f c f", spawn "flameshot full -c")
  --, ("M-S-f f s", spawn "flameshot gui")
  --, ("M-S-f g", spawn "flameshot launcher")
  --, ("<XF86LaunchB>", spawn "flameshot gui")
  --, ("M-S-f x", spawn "xfce4-screenshooter")
  ]
   where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
         nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))



main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar"
  xmonad $ docks $ ewmhFullscreen $ ewmh def
        { manageHook = insertPosition End Newer 
                       <+> myManageHook 
                       <+> manageDocks
        --, handleEventHook = serverModeEventHookCmd
        --                <+> serverModeEventHook 
        --                <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
        --                <+> minimizeEventHook
        -- , handleEventHook = fullscreenEventHook

        , modMask = myModMask
        , startupHook = myStartupHook
        , layoutHook = showWName' myShowWNameTheme $ myLayoutHook
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
              { ppOutput = \x -> hPutStrLn xmproc0 x
              , ppCurrent = xmobarColor color06 "" . wrap "[" "]"
              , ppVisible = xmobarColor color05 "" . clickable
              , ppHidden = xmobarColor color04 "" . wrap
                           ("<fc=" ++ color05 ++ ">") "</fc>" . clickable
              , ppHiddenNoWindows = xmobarColor colorForeAlt ""  . clickable
              , ppTitle = xmobarColor color14 "" . shorten 70
              , ppSep =  "<fc=" ++ color09 ++ "> | </fc>"
              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
              , ppExtras = [windowCount]
              , ppOrder  = \(ws:l:t:ex) -> [ws] ++ ex ++ ["<fc=" ++ color05 ++ ">[" ++ l ++ "]</fc> " ++ t ]
              }

        } `additionalKeysP` myKeys
