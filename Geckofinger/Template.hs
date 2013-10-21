module Geckofinger.Template (
  generateConfig,
  configFromArgs
) where

import Text.StringTemplate
import Data.Aeson (decode)
import Data.Map as Map (assocs)
import qualified Data.ByteString.Lazy as LBS (readFile, ByteString)
import Control.Exception (try)
import System.IO (stderr, hPutStrLn)

configFromArgs :: [String] -> IO (Maybe String)
-- at least two arguments needed
configFromArgs (tpl:vars:_) = do
  template <- safeReadFile tpl
  values <- safeReadFileLBS vars
  case template of
    Nothing -> return Nothing
    Just t -> do
      case values of
        Nothing -> return Nothing
        Just v -> do
          return $ generateConfig t v
configFromArgs _ = do
  hPutStrLn stderr "Wrong number of arguments" -- TODO better logging
  return Nothing

-- provide the raw string to create a template and the json values
-- as lazy bytestring
generateConfig :: String -> LBS.ByteString -> Maybe String
generateConfig tpl jsonv = do
  case decode jsonv of
    Just v -> return $ renderWithValues (assocs v) (newSTMP tpl)
    Nothing -> Nothing -- TODO log something
  where
    renderWithValues :: [(String, String)] -> StringTemplate String -> String
    renderWithValues vmap = toString . (setManyAttrib vmap)

-- give try a type signature to make ghc happy
try' :: IO a -> IO (Either IOError a)
try' = try

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile fp = do
  content <- try' (readFile fp)
  case content of
    Left _ -> do
      hPutStrLn stderr $ "Failed to open " ++ fp -- TODO better logging
      return Nothing
    Right c -> return $ Just c

safeReadFileLBS :: FilePath -> IO (Maybe LBS.ByteString)
safeReadFileLBS fp = do
  content <- try' (LBS.readFile fp)
  case content of
    Left _ -> do
      hPutStrLn stderr $ "Failed to open " ++ fp -- TODO better logging
      return Nothing
    Right c -> return $ Just c
