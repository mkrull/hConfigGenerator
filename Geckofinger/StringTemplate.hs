module Geckofinger.StringTemplate (
  mergeSourceStrings,
  mergeSourceFiles
) where

import Text.StringTemplate
import Data.Aeson (decode)
import Data.Map as Map (assocs)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import Geckofinger.FileIO (safeReadFile, safeReadFileLBS)

-- provide a tuple of two filenames and a bool to indicate the
-- type of template used
-- filenames must be in order: template file and json file
mergeSourceFiles :: (String, String, Bool) -> IO (Maybe String)
mergeSourceFiles (tpl, vars, angle) = do
  template <- safeReadFile tpl
  values <- safeReadFileLBS vars
  case template of
    Nothing -> return Nothing
    Just t -> case values of
        Nothing -> return Nothing
        Just v -> return $ mergeSourceStrings t v angle
mergeSourceFiles _ = return Nothing

-- provide the raw string to create a template and the json values
-- as lazy bytestring
-- provide a boolean value to indicate if the template should
-- use angle brackets instead of the default dollar sign delimiters
mergeSourceStrings :: String -> LBS.ByteString -> Bool -> Maybe String
mergeSourceStrings tpl jsonv angle = case decode jsonv of
    Just v -> return $ renderWithValues (assocs v) (newTpl angle tpl)
    Nothing -> Nothing -- TODO log something
  where
    newTpl :: Bool -> String -> StringTemplate String
    newTpl True = newAngleSTMP
    newTpl _ = newSTMP
    renderWithValues :: [(String, String)] -> StringTemplate String -> String
    renderWithValues vmap = toString . setManyAttrib vmap
