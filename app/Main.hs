module Main where

import Control.Concurrent.Async
import Control.Monad
import Network.Socket
import Network.Socket.ByteString
import Options.Applicative
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
    let addr = head addrs
    sock <- openSocket addr
    bind sock (addrAddress addr)
    return sock

recvAndPrint sock =
  forever $ do
    (bs, sa) <- recvFrom sock 1500
    putStrLn $ "read from " ++ show sa ++ ": " ++ show bs

listenDoThenWait job = do
  sock <- createSocket
  receiver <- async $ recvAndPrint sock
  job sock
  wait receiver

ping = listenDoThenWait print

listen = listenDoThenWait $ void . pure
