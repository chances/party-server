module Main where

import App (run)

import System.Environment (getEnvironment)
import System.IO (hSetBuffering, IO(..), BufferMode(LineBuffering), stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
    run port
