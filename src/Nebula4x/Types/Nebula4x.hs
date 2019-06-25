{-# LANGUAGE 	GeneralizedNewtypeDeriving #-}
{-# LANGUAGE 	DeriveFunctor #-}

module Nebula4x.Types.Nebula4x where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.IORef
import           System.Random

import           Nebula4x.Types.GameState

-- Disclaimer: I have no idea how monad transformers work.
-- TODO: This was copy-pasted and hacked together - should probably be revisited.

-- Using IORef instead of State/ST/MVar for maximum performance.
-- I could be wrong though, still needs some testing.

type GameStateRef = IORef GameState
type GameLogs = [String]
type Nebula4xMonadStack = ReaderT GameStateRef (WriterT GameLogs IO)
newtype Nebula4x a = Nebula4x { runN :: Nebula4xMonadStack a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GameStateRef, MonadWriter GameLogs)

runNebula4x :: Nebula4x a -> GameState -> IO (a, GameLogs)
runNebula4x k gameState = do
  stateRef <- newIORef gameState
  runWriterT (runReaderT (runN k) stateRef)

-- 
-- Various helper functions for working in the Nebula4x monad.
--
-- They were put in this file for convenience (only need the Nebula4x.Types import).
--

getGameStateRef :: Nebula4x (GameStateRef)
getGameStateRef = ask

getGameState :: Nebula4x GameState
getGameState = do
  gsr <- getGameStateRef
  liftIO (readIORef gsr)

setGameState :: GameState -> Nebula4x ()
setGameState gs = do
  gsr <- getGameStateRef
  liftIO (atomicWriteIORef gsr gs)

modifyGameState :: (GameState -> GameState) -> Nebula4x ()
modifyGameState f = do
  gs <- getGameState
  setGameState (f gs)

modifyAndGetGameState :: (GameState -> GameState) -> Nebula4x GameState
modifyAndGetGameState f = do
  gs <- getGameState
  let newGS = f gs
  setGameState newGS
  return newGS

getGen :: Nebula4x StdGen
getGen = do
  gameState <- getGameState
  return $ read (view randomSeed gameState)

setGen :: StdGen -> Nebula4x ()
setGen gen = modifyGameState (set randomSeed $ show gen)

addLog :: String -> Nebula4x ()
addLog = tell . return