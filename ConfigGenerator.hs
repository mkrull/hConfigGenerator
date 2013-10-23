module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import qualified Geckofinger.StringTemplate as ST

main :: IO ()
main = do
  args <- getArgs
  config <- ST.mergeSourceFiles $ templateArgs args
  case config of
    Just c -> success c
    Nothing -> failure "Failed to generate config"
  where
    success :: String -> IO ()
    success s = do
      putStrLn s
      exitSuccess
    failure :: String -> IO ()
    failure s = do
      hPutStrLn stderr s
      exitFailure

templateArgs :: [String] -> (String, String, Bool)
templateArgs (tpl:json:"angle":_) = (tpl, json, True)
templateArgs (tpl:json:_) = (tpl, json, False)
