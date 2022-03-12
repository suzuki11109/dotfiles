-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

-- Base
import XMonad
import System.Exit
import qualified XMonad.StackSet as W

-- Data
import Data.Monoid
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.IO (hPutStrLn)

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.FloatKeys
import XMonad.Actions.Promote

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.BinaryColumn
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition

-- Utils
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.Util.Hacks as Hacks
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- XF86 keys
import Graphics.X11.ExtraTypes.XF86

import Colors.DoomOne

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 2

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myWorkspaces    = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myEmacs = "emacsclient -c "

-- toggle xmobar command
--
toggleXmobar :: String
toggleXmobar = "dbus-send --session --dest=org.Xmobar.Control --type=method_call '/org/Xmobar/Control' org.Xmobar.Control.SendSignal 'string:Toggle 0'"

toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else (W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s)
    )

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: [(String, X ())]
myKeys =
    -- KB_GROUP WM
  [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart")             -- Restart XMonad
  , ("M-S-q", io exitSuccess)                       -- Exit XMonad

  -- KB_GROUP Floating Windows
  --, ("M-M1-<Left>",  withFocused (keysResizeWindow (10,0) (1,1)))
  --, ("M-M1-<Right>", withFocused (keysResizeWindow (-10,0) (1,1)))
  --, ("M-M1-<Up>",    withFocused (keysResizeWindow (0,10) (1,1)))
  --, ("M-M1-<Down>",  withFocused (keysResizeWindow (0,-10) (1,1)))
  --, ("M-M1-S-<Left>",   withFocused (keysMoveWindow (-10,0)))
  --, ("M-M1-S-<Right>",  withFocused (keysMoveWindow (10,0)))
  --, ("M-M1-S-<Up>",     withFocused (keysMoveWindow (0,-10)))
  --, ("M-M1-S-<Down>",   withFocused (keysMoveWindow (0,10)))

  , ("M-p", spawn "dmenu_run -fn 'JetBrainsMono-10'")

  -- KB_GROUP Application launchers
  , ("M-<Return>", spawn (myTerminal))                        -- Spawn terminal

  -- KB_GROUP Kill
  , ("M-q", kill1)    -- Kill selected window
  , ("M-S-a", killAll)  -- Kill all windows in workspace

  -- KB_GROUP Float/Tile
  , ("M-f", withFocused toggleFloat) -- Toggle floating layout
  , ("M-t", withFocused $ windows . W.sink)  -- Tile selected floating window
  , ("M-S-t", sinkAll)                       -- Tile all floating windows

  -- KB_GROUP Clients
  , ("M-m", windows W.focusMaster)                     -- Move focus to the master window
  , ("M-j", windows W.focusDown)                       -- Move focus to the next window
  , ("M-k", windows W.focusUp)                         -- Move focus to the prev window
  , ("M-S-j", windows W.swapDown)                -- Swap focused window with next window
  , ("M-S-k", windows W.swapUp)                  -- Swap focused window with prev window
  -- , ("M-C-m", windows W.swapMaster)              -- Swap the focused window and the master window
  , ("M-C-m", promote)                   -- Moves focused window to master, others maintain order

  , ("M-S-<Up>", sendMessage (IncMasterN 1))        -- Increase # of clients master pane
  , ("M-S-<Down>", sendMessage (IncMasterN (-1)))     -- Decrease # of clients master pane
  , ("M-C-<Up>", increaseLimit)                     -- Increase # of windows
  , ("M-C-<Down>", decreaseLimit)                     -- Decrease # of windows
  , ("M-h", sendMessage Shrink)                        -- Shrink horiz window width
  , ("M-l", sendMessage Expand)                        -- Expand horiz window width
  -- , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
  -- , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width

  -- KB_GROUP Scratchpads
  -- , ("M-s t", namedScratchpadAction myScratchPads "terminal")
  -- , ("M-s f", namedScratchpadAction myScratchPads "fm")
  -- --, ("M-s o", namedScratchpadAction myScratchPads "obs")
  -- , ("M-s s", namedScratchpadAction myScratchPads "sysmon")
  -- , ("M-s c", namedScratchpadAction myScratchPads "calculator")

  -- KB_GROUP Window Spacing
  -- , ("M-M1-<Up>", decWindowSpacing 4)         -- Decrease window spacing
  -- , ("M-M1-<Down>", incWindowSpacing 4)         -- Increase window spacing

  -- KB_GROUP Layouts
  , ("M-<Space>", sendMessage NextLayout)                                     -- Go to next layout
  --, ("M1-S-<Tab>", sendMessage FirstLayout)                                     -- Go to next layout
  -- , ("M-S-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggle struts
  -- , ("M-C-f", sendMessage (MT.Toggle NBFULL))
  -- , ("M-S-b", sendMessage ToggleStruts)

   -- KB_GROUP Workspace
  , ("M-]", nextWS) -- Go to next workspace
  , ("M-[", prevWS) -- Go to previous workspace

  -- KB_GROUP Emacs
  , ("M-e", spawn (myEmacs))
  --, ("M-e M-e", spawn (myEmacs ++ ("--eval '(dashboard-refresh-buffer)'")))                     -- emacs dashboard
  --, ("M-e b", spawn (myEmacs ++ ("--eval '(ibuffer)'")))                                      -- list buffers
  --, ("M-e d", spawn (myEmacs ++ ("--eval '(dired nil)'")))                                    -- dired
  --, ("M-e i", spawn (myEmacs ++ ("--eval '(erc)'")))                                          -- erc irc client
  --, ("M-e n", spawn (myEmacs ++ ("--eval '(elfeed)'")))                                       -- elfeed rss
  --, ("M-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))                                       -- eshell
  --, ("M-e v", spawn (myEmacs ++ ("--eval '(+vterm/here nil)'")))                              -- vterm if on Doom Emacs
  --, ("M-e w", spawn (myEmacs ++ ("--eval '(doom/window-maximize-buffer(eww \"gnu.org\"))'"))) -- eww browser


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

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayout = smartBorders
           $ avoidStruts
           $ tall ||| full
  where
     tall   = renamed [Replace "Tall"]
                $ mySpacing 5
                $ (Tall 1 delta ratio)
     full    = renamed [Replace "Full"] Full

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
--
myManageHook = fmap not willFloat --> insertPosition Below Newer <+> composeAll
    [ className =? "Gimp"               --> doFloat
    , className =? "jetbrains-toolbox"  --> doIgnore
    , className =? "feh"                --> doFloat
    , className =? "Galculator"         --> doFloat
    , className =? "TeamSpeak 3"        --> doFloat
    , className =? "Insomnia"           --> doFloat
    , title     =? "win0"               --> doFloat
    , title     =? "Contrôle du volume" --> doFloat
    , resource  =? "desktop_window"     --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = Hacks.trayerAboveXmobarEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    setDefaultCursor xC_left_ptr -- set default cursor
    spawnOnce "picom -b"
    -- spawnOnce "gnome-keyring-daemon --start -d"
    -- spawnOnce "redshift -c /home/pierre/.config/redshift/redshift.conf"
    -- spawn "killall trayer"
    spawnOnce "trayer --edge top --align right --widthtype request --transparent true --alpha 0 --expand true --SetPartialStrut true --SetDockType true --tint 0x282c34 --height 50 "
    spawnOnce "blueberry-tray"
    -- spawnOnce "pasystray"
    -- spawnOnce "powerkit"
    spawnOnce "emacs --daemon"
    spawnOnce "easyeffects --gapplication-service"
    spawnOnce "dunst"
    spawn "~/.fehbg &"
    spawnOnce "xfce4-power-manager"
    spawn "nm-applet --sm-disable"
    spawn "sleep 2 && fcitx5 --replace -d"
    -- spawnOnce "udiskie"
    -- spawnOnce "bato"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar"
    xmonad
     . docks
     . ewmhFullscreen
     . ewmh
     $ def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = color01,
        focusedBorderColor = color06,

      -- key bindings
        -- mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = dynamicLogWithPP $ xmobarPP
              { ppOutput = \x -> hPutStrLn xmproc0 x
              , ppCurrent = xmobarColor color06 "" . wrap "[" "]"
              , ppVisible = xmobarColor color05 "" . clickable
              , ppHidden = xmobarColor color04 "" . wrap
                           ("<fc=" ++ color05 ++ ">") "</fc>" . clickable
              , ppHiddenNoWindows = xmobarColor colorFore ""  . clickable
              , ppTitle = xmobarColor color14 "" . shorten 70
              , ppSep =  "<fc=" ++ color09 ++ "> | </fc>"
              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
              , ppExtras = [windowCount]
              , ppOrder  = \(ws:l:t:ex) -> [ws] ++ ex ++ ["<fc=" ++ color05 ++ ">[" ++ l ++ "]</fc> " ++ t ]
              }
    } `additionalKeysP` myKeys
