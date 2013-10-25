module Main where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import Data.List
import qualified Geckofinger.StringTemplate as ST

-- flag datatype
-- derive Eq and Ord to sort command line arguments
data Flag = TemplateFile String | JSONFile String | AngleTemplate
                                      deriving (Show, Eq, Ord)

-- description of the command line parameters
options :: [OptDescr Flag]
options = [
          Option "t" ["template"] (ReqArg TemplateFile "<filename>") "StringTemplate file",
          Option "j" ["json"] (ReqArg JSONFile "<filename>") "JSON file",
          Option "a" ["angle"] (NoArg AngleTemplate) "Template uses angles instead of dollar delimiters"
          ]

-- parse arguments into a list of flags
parseArgs :: [String] -> String -> IO [Flag]
parseArgs argv name =
   case getOpt RequireOrder options argv of
      (o,_,[]  ) -> return o
      (_,_,errs) -> ioError (userError (concat errs ++ info name))

info :: String -> String
info n = usageInfo (header n) options
  where
    header :: String -> String
    header name = "Usage: " ++ name ++ " -t|--template <templateFile> -j|--json <jsonFile> [-a|--angle]"

main :: IO ()
main = do
  -- get command line arguments, program name
  -- and parse arguments into a list of flags
  args <- getArgs
  name <- getProgName
  opts <- parseArgs args name
  case tupleifyOpts $ sort opts of
    Just o -> do
        -- generate string from template file and json file
        config <- ST.mergeSourceFiles o
        case config of
          Just c -> success c
          Nothing -> failure "Failed to generate config"
    -- if list of flags does fall through tupleifyOpts
    -- some parameters are missing
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

-- Nothing if not at least a template file and a json file
-- are given on the command line
tupleifyOpts :: [Flag] -> Maybe (String, String, Bool)
tupleifyOpts (TemplateFile t:JSONFile j:[]) = Just (t, j, False)
tupleifyOpts (TemplateFile t:JSONFile j:AngleTemplate:[]) = Just (t, j, True)
tupleifyOpts _ = Nothing
