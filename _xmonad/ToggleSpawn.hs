{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ToggleSpawn (
    toggleSpawn
    ) where


import Control.Monad (liftM)
import System.Posix.Types (ProcessID)
import qualified Data.Map as M

import XMonad
import qualified XMonad.Util.ExtensibleState as XS


type State = M.Map String ProcessID

newtype TSStorage = TSStorage { runTSStorage :: State }
    deriving (Typeable,Read,Show)

instance ExtensionClass TSStorage where
    initialValue = TSStorage M.empty
    extensionType = PersistentExtension

getState :: X State
getState = liftM runTSStorage XS.get

putState :: State -> X ()
putState = XS.put . TSStorage


toggleSpawn :: String -> X ()
toggleSpawn cmd = do
    state <- getState
    case M.lookup cmd state of
        Just pid -> spawn ("kill " ++ show pid)
                    >> putState (M.delete cmd state)
        Nothing -> spawnPID cmd
                    >>= putState . insert cmd state
                    where insert k = flip $ M.insert k
