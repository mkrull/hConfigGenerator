module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import Data.List
import qualified Geckofinger.StringTemplate as ST

data Flag = TemplateFile String | JSONFile String | AngleTemplate
                                      deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = [
          Option "t" ["template"] (ReqArg TemplateFile "<filename>") "StringTemplate file",
          Option "j" ["json"] (ReqArg JSONFile "<filename>") "JSON file",
          Option "a" ["angle"] (NoArg AngleTemplate) "Template uses angles instead of dollar delimiters"
          ]

parseArgs :: [String] -> String -> IO ([Flag], [String])
parseArgs argv name =
   case getOpt RequireOrder options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ info name))

info :: String -> String
info n = usageInfo (header n) options
  where
    header :: String -> String
    header name = "Usage: " ++ name ++ " -t|--template <templateFile> -j|--json <jsonFile> [-a|--angle]"

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  opts <- parseArgs args name
  case tupleifyOpts $ sort $ fst opts of
    Just o -> do
        config <- ST.mergeSourceFiles o
        case config of
          Just c -> success c
          Nothing -> failure "Failed to generate config"
    Nothing -> failure $ info name
  where
    success :: String -> IO ()
    success s = do
      putStrLn s
      exitSuccess
    failure :: String -> IO ()
    failure s = do
      hPutStrLn stderr s
      exitFailure

tupleifyOpts :: [Flag] -> Maybe (String, String, Bool)
tupleifyOpts (TemplateFile t:JSONFile j:[]) = Just (t, j, False)
tupleifyOpts (TemplateFile t:JSONFile j:AngleTemplate:[]) = Just (t, j, True)
tupleifyOpts _ = Nothing
