{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.BEncode
import Data.BEncode.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text.Encoding as E
import Network.Socket
import Network.Socket.ByteString
import Options.Applicative
import System.Random
import Text.Pretty.Simple
import BitTorrent.DHT

pingParser :: Parser (IO ())
pingParser = ping <$> optional (option auto (short 'c' <> long "count"))

commands =
  hsubparser
    ( command "ping" (info pingParser idm)
        <> command
          "listen"
          (info (pure Main.listen) (progDesc "just listens for incoming messages"))
    )

main :: IO ()
main = join $ execParser (info (commands <**> helper) idm)

createSocket hints =
  withSocketsDo $ do
    addrs <-
      getAddrInfo
        ((\h -> h {addrFlags = AI_PASSIVE : addrFlags h}) <$> hints)
        Nothing
        (Just "42069")
    pPrint addrs
    let addr = head addrs
    sock <- openSocket addr
    bind sock (addrAddress addr)
    return sock

recvAndPrint sock =
  forever $ do
    (bs, sa) <- recvFrom sock 1500
    print bs
    pPrint
      ("read from", show sa, C.unpack . LB.fromStrict $ bs, prettifyPacket bs)
  where
    prettifyPacket bs = decode bs :: Result BValue

type Hints = Maybe AddrInfo

listenDoThenWait :: (Socket -> Hints -> IO a) -> Hints -> IO ()
listenDoThenWait job hints = do
  sock <- createSocket hints
  receiver <- async $ recvAndPrint sock
  job sock hints
  wait receiver

globalAddrs =
  [ ("router.utorrent.com", "6881"),
    ("router.bittorrent.com", "6881"),
    ("dht.transmissionbt.com", "6881"),
    ("dht.aelitis.com", "6881"), -- Vuze
    ("router.silotis.us", "6881"), -- IPv6
    ("dht.libtorrent.org", "25401") -- @arvidn's
  ]

getAddrAddress :: (HostName, ServiceName) -> Hints -> IO (Maybe SockAddr)
getAddrAddress (host, service) hints = do
  addrs <- try $ getAddrInfo hints (Just host) (Just service)
  case addrs of
    Left (e :: IOException) -> do
      print ["error resolving", show (host, service), show e]
      pure Nothing
    Right ok -> do
      pPrint addrs
      return . Just . addrAddress . Prelude.head $ ok

mainHints =
  Just $ defaultHints {addrSocketType = Datagram, addrFamily = AF_INET}

globalAddrAddresses ::
  Maybe AddrInfo -> IO [((HostName, ServiceName), SockAddr)]
globalAddrAddresses hints = do
  nameAddrsIO <- zip globalAddrs <$> mapM (`getAddrAddress` hints) globalAddrs
  return
    [ (name, fromJust maybeAddr)
      | (name, maybeAddr) <- nameAddrsIO,
        isJust maybeAddr
    ]

type Repeats = Int

ping :: Maybe Repeats -> IO ()
ping repeats = listenDoThenWait sendPing mainHints
  where
    sendPing :: Socket -> Hints -> IO ()
    sendPing sock hints = do
      stdGen <- getStdGen
      (id, stdGen) <- pure $ genByteString 20 stdGen
      let mkBuf :: (RandomGen g) => g -> (B.ByteString, g)
          mkBuf g =
            let (t, g') = genByteString 4 g
             in ( LB.toStrict . encode $
                    "t" .=! t
                      .: "y" .=! ("q" :: BString)
                      .: "q" .=! ("ping" :: BString)
                      .: "a" .=! ("id" .=! id .: endDict)
                      .: endDict,
                  g'
                )
      addrs <- globalAddrAddresses hints
      let theDo =
            loop $ do
              forM_ addrs $ \addr -> do
                g <- get
                let (buf, g') = mkBuf g
                put g'
                pPrint buf
                liftIO $ sendTo sock buf $ snd addr
              liftIO $ threadDelay 5e6
      void . forkIO . void $ runStateT theDo stdGen
    loop = maybe forever replicateM_ repeats

listen = listenDoThenWait (\sock hints -> pure ()) mainHints
