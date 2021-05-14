{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad (liftM)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import Data.Ratio
import System.IO
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.Plane
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.Run (spawnPipe)

import WorkspaceColours

mMask :: KeyMask
mMask = mod4Mask

font :: String
font = "Liberation Sans:pixelsize=15:antialias=true:autohint=true"

gsConfig :: HasColorizer a => GSConfig a
gsConfig = def { gs_font = "xft:" ++ font }

bmDir :: String
bmDir = "/home/iross/.xmonad/dzen2"

mainBar :: String
mainBar =
  "dzen2 -fn '" ++ font ++ "' -dock -x '0' -y '0' -h '24' -w '1350' "
    ++ "-ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"

conkyBar :: String
conkyBar =
  "conky -c /home/iross/.xmonad/conky_dzen | "
    ++ "dzen2 -fn '"
    ++ font
    ++ "' -dock -x '1350' -w '420' -h '24' -ta 'r' "
    ++ "-bg '#1B1D1E' -fg '#FFFFFF' -y '0'"

icon :: String -> String
icon name = "^i(" ++ bmDir ++ "/" ++ name ++ ".xbm)"

wsColoursConf :: WorkspaceColoursConf
wsColoursConf = WorkspaceColoursConf
  { defaultColour = "alice blue"
  , colours =
    WorkspaceColours [ ("1", "bisque"), ("2", "light sea green"), ("3", "powder blue")
                     , ("4", "light salmon"), ("5", "sea green"), ("6", "royal blue")
                     , ("7", "salmon"), ("8", "dark sea green"), ("9", "steel blue") ]
  }

myLogHook :: Handle -> X ()
myLogHook h = do
  dynamicLogWithPP $
    def
      { ppCurrent = dzenColor "#ebac54" "#1B1D1E" . (' ' :),
        ppVisible = dzenColor "#00ddbb" "#1B1D1E" . (' ' :),
        ppHidden = dzenColor "white" "#1B1D1E" . (' ' :),
        ppHiddenNoWindows =
          dzenColor "#7b7b7b" "#1B1D1E" . (' ' :),
        ppUrgent = dzenColor "#ff0000" "#1B1D1E" . (' ' :),
        ppWsSep = "",
        ppSep = " | ",
        ppLayout =
          dzenColor "#ebac54" "#1B1D1E"
            . ( \x -> case x of
                  "Tall" -> icon "tall"
                  "Mirror Tall" -> icon "mtall"
                  "Full" -> icon "full"
                  "TwoPane" -> icon "half"
                  ('T':'a':'b':'b':'e':'d':_) -> icon "mouse"
                  "Simple Float" -> "~"
                  _ -> x
              ),
        ppTitle =
          (" " ++)
            . dzenColor "white" "#1B1D1E"
            . dzenEscape,
        ppOutput = hPutStrLn h
      }
  workspaceColours wsColoursConf

wwin :: X ()
wwin = warpToWindow (19 % 20) (19 % 20)

main :: IO ()
main = do
  dzenLeft <- spawnPipe mainBar
  spawnPipe conkyBar
  xmonad $
    docks $
      def
        { borderWidth = 3,
          focusedBorderColor = "orange",
          terminal = "kitty",
          modMask = mMask,
          focusFollowsMouse = False,
          manageHook = manageDocks <+> specialManageHook <+> manageHook def,
          layoutHook = myLayoutHook,
          startupHook = setWMName "LG3D",
          logHook = myLogHook dzenLeft,
          workspaces = map show [1 .. 12 :: Int]
        }
        `additionalKeys` ( [ ((mMask .|. shiftMask, xK_l), spawn "gnome-screensaver-command -l"),
                             ((mMask, xK_f), sendMessage ToggleStruts >> sendMessage ToggleLayout),
                             ((mMask, xK_g), goToSelected gsConfig),
                             ((mMask, xK_s), spawnSelected gsConfig appList),
                             ((mMask, xK_F10), spawn "flameshot gui"),
                             ((mMask, xK_p), spawn $ "dmenu_run -fn '" ++ font ++ "'")
                           ]
                             ++ [ ( (shiftMask .|. mMask, key),
                                    do
                                      ws <- screenWorkspace sc
                                      whenJust ws (windows . W.shift)
                                  )
                                  | (key, sc) <- zip [xK_e, xK_w] [0 ..]
                                ]
                             ++ [ ( (mMask, key),
                                    screenWorkspace sc
                                      >>= \case
                                        Nothing -> return ()
                                        Just ws -> windows (W.view ws) >> warpToScreen sc (1 % 2) (1 % 2)
                                  )
                                  | (key, sc) <- zip [xK_e, xK_w] [0 ..]
                                ]
                             ++ [ ((mMask, xK_Tab), windows W.focusDown >> wwin),
                                  ((mMask .|. shiftMask, xK_Tab), windows W.focusUp >> wwin),
                                  ((mMask, xK_j), windows W.focusDown >> wwin),
                                  ((mMask, xK_k), windows W.focusUp >> wwin),
                                  ((mMask, xK_m), windows W.focusMaster >> wwin)
                                ]
                             ++ M.toList (planeKeys mMask (Lines 4) Finite)
                         )
        `additionalMouseBindings` [ ( (mMask .|. shiftMask, button1),
                                      \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
                                    )
                                  ]

-- resizeForScreencast :: Window -> X ()
-- resizeForScreencast w = whenX (isClient w) $ withDisplay $ \d -> do
--   io $ raiseWindow d w
--   io $ moveWindow d w 0 0
--   io $ resizeWindow d w 1280 720
--   float w

myLayoutHook =
  avoidStruts $
    toggleLayouts (noBorders Full) (tiled ||| Mirror tiled ||| Full ||| tabs ||| twopane)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100
    twopane = TwoPane (3/100) (1/2)
    tabs = tabbed shrinkText def { fontName = "xft:" ++ font
                                 , decoHeight = 32
                                 , activeColor = "#FFA500"
                                 , activeBorderColor = "#FFA500"
                                 , activeTextColor = "#000000" }

specialManageHook =
  composeAll . concat $
    [ [isDialog --> doFloat],
      [className =? c --> doFloat | c <- myCFloats],
      [title =? t --> doFloat | t <- myTFloats],
      [title `isPrefixedBy` tp --> doFloat | tp <- myTPrefixFloats],
      [className `isSuffixedBy` ".py" --> doFloat],
      [resource =? r --> doFloat | r <- myRFloats],
      [  qNot isDialog <&&> ((x `isInfixOf`) <$> className) --> doShift ws
        | (ws, xs) <- myShifts,
          x <- xs
      ],
      [  qNot isDialog
          <&&> (className =? x <||> title =? x <||> resource =? x) --> doShift ws
        | (ws, xs) <- myShifts,
          x <- xs
      ]
    ]
  where
    myCFloats = ["Python2", "VirtualBox", "Teensy"]
    myTFloats =
      [ "R Graphics",
        "v4l:// - VLC media player",
        "Android Emulator",
        "CoAP LED Controller"
      ]
    myTPrefixFloats = []
    myRFloats = []
    myShifts = []

isPrefixedBy :: Query String -> String -> Query Bool
q `isPrefixedBy` x = fmap (x `isPrefixOf`) q

isSuffixedBy :: Query String -> String -> Query Bool
q `isSuffixedBy` x = fmap (x `isSuffixOf`) q

qNot :: Monad m => m Bool -> m Bool
qNot = fmap not

appList :: [String]
appList =
  [ "emacs",
    "vivaldi",
    "libreoffice",
    "gimp",
    "inkscape",
    "evince",
    "kicad",
    "freecad",
    "vlc",
    "ltspice",
    "openshot-qt",
    "slack"
  ]
