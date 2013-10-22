module Geckofinger.FileIO (
  safeReadFile,
  safeReadFileLBS
) where

import qualified Data.ByteString.Lazy as LBS (readFile, ByteString)
import Control.Exception (try)

-- give try a type signature to make ghc happy
try' :: IO a -> IO (Either IOError a)
try' = try

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile fp = do
  content <- try' (readFile fp)
  case content of
    -- failed to open file
    Left _ -> return Nothing
    -- return content
    Right c -> return $ Just c

safeReadFileLBS :: FilePath -> IO (Maybe LBS.ByteString)
safeReadFileLBS fp = do
  content <- try' (LBS.readFile fp)
  case content of
    -- failed to open file
    Left _ -> return Nothing
    -- return content
    Right c -> return $ Just c
