{-# LANGUAGE RecordWildCards #-}

module BitTorrent.DHT.Traversal where

import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.ByteString
import Data.Set
import Network.Socket

data Traversal =
  Traversal
    { pendingAddrs :: Set SockAddr
    , triedAddrs :: Set SockAddr
    , alpha :: Int
    , pending :: Int
    , stopping :: Bool
    }

type NodeId = ByteString

startTraversal ::
     [SockAddr] -> NodeId -> NodeId -> Int -> (ByteString -> IO ()) -> Traversal
startTraversal = undefined

-- startTraversal initialAddrs targetId localId alpha send
sender state =
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
            { pending = pending + 1
            , pendingAddrs = delete addr pendingAddrs
            , triedAddrs = insert addr triedAddrs
            }
        return . Just $ addr
