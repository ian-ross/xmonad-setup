{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------
module WorkspaceColours
  ( workspaceColours
  , WorkspaceColoursConf(..)
  , WorkspaceColours(..)
  , defWorkspaceColoursConf
  ) where

import XMonad
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

import System.IO
import System.Process

import qualified Data.Map as M
import Data.List (intersperse, sortOn, intercalate)

import Control.Arrow ((&&&))
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup

-- Add a log hook like this:
--
-- > myWorkspaces = ["1:main","2:misc","3","4"]
-- > ...
-- > main = xmonad $ defaultConfig {
-- >   logHook = workspaceColours def
-- >     { colours = WorkspaceColours [ ("1:main", "orange")
-- >                                  , ("2:misc", "white") ]
-- >     , defaultColour = "black"
-- >     }
-- >   }
-- > ...

data WCState = WCState (Maybe [WorkspaceId]) (Maybe ProcessHandle) deriving Typeable
instance ExtensionClass WCState where
  initialValue = WCState Nothing Nothing

newtype WorkspaceColours = WorkspaceColours [(WorkspaceId, String)]
  deriving (Show,Read)

instance Monoid WorkspaceColours where
  mempty = WorkspaceColours []
  mappend (WorkspaceColours w1) (WorkspaceColours w2) =
    WorkspaceColours $ M.toList $ M.fromList w2 `M.union` M.fromList w1

instance Semigroup WorkspaceColours where
  (<>) = mappend

data WorkspaceColoursConf = WorkspaceColoursConf {
    defaultColour :: String
  , colours :: WorkspaceColours
  } deriving (Show, Read)

defWorkspaceColoursConf :: WorkspaceColoursConf
defWorkspaceColoursConf = WorkspaceColoursConf "gray" $ WorkspaceColours []

instance Default WorkspaceColoursConf where
    def = defWorkspaceColoursConf

workspaceColours :: WorkspaceColoursConf -> X ()
workspaceColours wcconf = do
  WCState oldws h <- XS.get
  visws <- getVisibleWorkspaces
  when (Just visws /= oldws) $ do

    wscols <- getColoursAndScreens wcconf

    case h of
      Nothing -> return ()
      Just pid -> liftIO $ terminateProcess pid

    handle <- applyWorkspaceColour wscols
    XS.put $ WCState (Just visws) $ Just handle


getVisibleWorkspaces :: X [WorkspaceId]
getVisibleWorkspaces = do
  winset <- gets windowset
  return $ map (S.tag . S.workspace) . sortOn S.screen $
    S.current winset : S.visible winset

getColoursAndScreens :: WorkspaceColoursConf -> X [(ScreenId, String)]
getColoursAndScreens (WorkspaceColoursConf defCol (WorkspaceColours cols)) = do
  winset <- gets windowset
  visws <- getVisibleWorkspaces
  let visscr = map ((S.tag . S.workspace) &&& S.screen) $
        S.current winset : S.visible winset
      getColour wsid = M.findWithDefault defCol wsid $ M.fromList cols
  return $ map (\(ws,s)->(s, getColour ws)) visscr

applyWorkspaceColour :: [(ScreenId, String)] -> X ProcessHandle
applyWorkspaceColour parts = do
  winset <- gets windowset
  let cmds = map xsetrootCommand parts
      cmd = intercalate " ; " cmds
  liftIO $ runCommand cmd

xsetrootCommand :: (ScreenId, String) -> String
xsetrootCommand (S sid, col) = "xsetroot -solid '" ++ col ++ "' -display :" ++ show sid
