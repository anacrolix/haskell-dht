{-# LANGUAGE NumDecimals, OverloadedStrings, ScopedTypeVariables
  #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.BEncode
import qualified Data.ByteString as B
import Data.ByteString.Lazy
import qualified Data.Map.Strict as Map
import Network.Socket
import Network.Socket.ByteString
import Options.Applicative
import System.Random
import Text.Pretty.Simple

commands =
  hsubparser
    (command "ping" (info (pure ping) idm) <>
     command
       "listen"
       (info (pure Main.listen) (progDesc "just listens for incoming messages")))

main :: IO ()
main = join $ execParser (info (commands <**> helper) idm)

createSocket =
  withSocketsDo $ do
    addrs <-
      getAddrInfo
        (Just $
         defaultHints
           {addrFlags = [AI_PASSIVE, AI_NUMERICHOST], addrSocketType = Datagram})
        Nothing
        (Just "42069")
    pPrint addrs
    let addr = addrs !! 1
    sock <- openSocket addr
    bind sock (addrAddress addr)
    return sock

recvAndPrint sock =
  forever $ do
    (bs, sa) <- recvFrom sock 1500
    Prelude.putStrLn $ "read from " ++ show sa ++ ": " ++ show bs

listenDoThenWait :: (Socket -> IO a) -> IO ()
listenDoThenWait job = do
  sock <- createSocket
  receiver <- async $ recvAndPrint sock
  job sock
  wait receiver

globalAddrs =
  [ ("router.utorrent.com", "6881")
  , ("router.bittorrent.com", "6881")
  , ("dht.transmissionbt.com", "6881")
  , ("dht.aelitis.com", "6881") -- Vuze
  , ("router.silotis.us", "6881") -- IPv6
  , ("dht.libtorrent.org", "25401") -- @arvidn's
  ]

getAddrAddress (host, service) = do
  addrs <-
    getAddrInfo
      (Just $ defaultHints {addrFlags = [AI_V4MAPPED]})
      (Just host)
      (Just service)
  pPrint addrs
  return . addrAddress . Prelude.head $ addrs

globalAddrAddresses :: IO [SockAddr]
globalAddrAddresses = mapM getAddrAddress globalAddrs

ping :: IO ()
ping = listenDoThenWait sendPing
  where
    sendPing :: Socket -> IO ()
    sendPing sock = do
      stdGen <- getStdGen
      let id = fromStrict . fst $ genByteString 20 stdGen
      let mkBuf :: (RandomGen g) => g -> (B.ByteString, g)
          mkBuf g =
            let (t, g) = genByteString 4 g
             in ( toStrict . bPack . BDict $
                  Map.fromList
                    [ ("t", BString . fromStrict $ t)
                    , ("y", BString "q")
                    , ("q", BString "ping")
                    , ("a", BDict . Map.fromList $ [("id", BString id)])
                    ]
                , g)
      addrs <- globalAddrAddresses
      let theDo =
            forever $ do
              forM_ addrs $ \addr -> do
                g <- get
                let (buf, g) = mkBuf g
                put g
                pPrint addr
                pPrint buf
                liftIO $ sendTo sock buf addr
              liftIO $ threadDelay 5e6
      void . forkIO . void $ evalStateT theDo stdGen

listen = listenDoThenWait $ void . pure
