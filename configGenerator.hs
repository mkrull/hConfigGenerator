module Main where

import System.IO (stdout, stderr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import Geckofinger.Template

main :: IO ()
main = do
  args <- getArgs
  config <- configFromArgs args
  case config of
    Just c -> success c
    Nothing -> failure "Failed to generate config"
  where
    success :: String -> IO ()
    success s = do
      hPutStrLn stdout s
      exitSuccess
    failure :: String -> IO ()
    failure s = do
      hPutStrLn stderr s
      exitFailure
