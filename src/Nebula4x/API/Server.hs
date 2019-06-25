{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Nebula4x.API.Server where

import           Control.Concurrent.STM        (TVar, atomically, modifyTVar',
                                                newTVarIO, readTVarIO)
import           Control.Monad.Reader          (MonadIO, MonadReader,
                                                MonadTrans, ReaderT, ask, lift,
                                                liftIO, runReaderT)
import           Data.Text.Lazy                (Text)
import           Network.Wai.Middleware.Static (addBase, noDots, staticPolicy,
                                                (>->))
import           Web.Scotty.Trans              (ActionT, ScottyT, middleware,
                                                scottyT)

import           Nebula4x.Types
import           Nebula4x.Utils                (corsified)

-- For testing purposes
import           Testing.TestGameState

newtype WebM a = WebM
  { runWebM :: ReaderT (TVar GameState) IO a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadReader (TVar GameState)
             )

type WebRoute = ScottyT Text WebM

type WebAction = ActionT Text WebM

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

readState :: WebM GameState
readState = ask >>= liftIO . readTVarIO

gets :: (GameState -> b) -> WebM b
gets f = f <$> readState

modify :: (GameState -> GameState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

startServer :: Int -> WebRoute () -> IO ()
startServer port routes = do
  gameState <-  testGameState
  sync <- newTVarIO gameState
  let runActionToIO m = runReaderT (runWebM m) sync
  scottyT port runActionToIO $ do
    middleware corsified
    routes

staticFiles :: WebRoute ()
staticFiles = middleware $ staticPolicy (noDots >-> addBase "src/Web/Static")
