module BitTorrent.DHT.Traversal where

import Data.Set
import Data.ByteString
import Network.Socket

data Traversal = Traversal
  { pendingAddrs :: Set,
    triedAddrs :: Set,
    alpha :: Int
  }

type NodeId = ByteString

startTraversal :: [SockAddr] -> NodeId -> NodeId -> Int -> (ByteString -> IO ()) -> Traversal
startTraversal = undefined
-- startTraversal initialAddrs targetId localId alpha send
