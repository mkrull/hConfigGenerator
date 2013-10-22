module Geckofinger.StringTemplate (
  mergeSourceStrings,
  mergeSourceFiles
) where

import Text.StringTemplate
import Data.Aeson (decode)
import Data.Map as Map (assocs)
import qualified Data.ByteString.Lazy as LBS (readFile, ByteString)
import Geckofinger.FileIO (safeReadFile, safeReadFileLBS)

mergeSourceFiles :: [String] -> IO (Maybe String)
-- at least two arguments needed
mergeSourceFiles (tpl:vars:_) = do
  template <- safeReadFile tpl
  values <- safeReadFileLBS vars
  case template of
    Nothing -> return Nothing
    Just t -> case values of
        Nothing -> return Nothing
        Just v -> return $ mergeSourceStrings t v
-- wrong number of arguments
mergeSourceFiles _ = return Nothing

-- provide the raw string to create a template and the json values
-- as lazy bytestring
mergeSourceStrings :: String -> LBS.ByteString -> Maybe String
mergeSourceStrings tpl jsonv = case decode jsonv of
    Just v -> return $ renderWithValues (assocs v) (newSTMP tpl)
    Nothing -> Nothing -- TODO log something
  where
    renderWithValues :: [(String, String)] -> StringTemplate String -> String
    renderWithValues vmap = toString . setManyAttrib vmap
