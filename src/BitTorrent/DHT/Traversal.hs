{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module BitTorrent.DHT.Traversal where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import qualified Data.ByteString as B
import Data.Set as Set
import Network.Socket

data Traversal = Traversal
  { pendingAddrs :: Set SockAddr,
    triedAddrs :: Set SockAddr,
    alpha :: Int,
    pending :: Int,
    stopping :: Bool
  }

type NodeId = B.ByteString

startTraversal ::
  [SockAddr] ->
  NodeId ->
  NodeId ->
  Int ->
  (B.ByteString -> IO ()) ->
  IO (TVar Traversal)
startTraversal initialAddrs targetId localId alpha send = do
  state <-
    newTVarIO $
      Traversal
        { pendingAddrs = fromList initialAddrs,
          triedAddrs = Set.empty,
          alpha,
          pending = 0,
          stopping = False
        }
  forkIO $ sender state
  pure state

sender :: TVar Traversal -> IO ()
sender state = do
  addr <-
    atomically $ do
      state'@Traversal {..} <- readTVar state
      if stopping
        then return Nothing
        else do
          unless (pending < alpha) retry
          addr <- maybe retry pure $ lookupMin pendingAddrs
          writeTVar
            state
            state'
              { pending = pending + 1,
                pendingAddrs = delete addr pendingAddrs,
                triedAddrs = insert addr triedAddrs
              }
          return . Just $ addr
  case addr of
    Nothing -> pure ()
    Just addr -> do
      putStrLn $ "query " ++ show addr
      sender state
