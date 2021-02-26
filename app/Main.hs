
module Main where

import Options.Applicative
import Control.Monad (join)

commands = hsubparser (command "ping" (info (pure ping) idm )
    <> command "listen" (info (pure listen) (progDesc "just listens for incoming messages"))
    )

main :: IO ()
main = join $ execParser (info (commands <**> helper) idm)

ping = putStrLn "Okay will ping!"
listen = putStrLn "will listen"
