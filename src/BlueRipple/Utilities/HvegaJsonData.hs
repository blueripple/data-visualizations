{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module BlueRipple.Utilities.HvegaJsonData
  (
    module BlueRipple.Utilities.HvegaJsonData
  ) where

import qualified Data.Aeson as A
import qualified Knit.Report as K
import qualified Path
import qualified System.Directory as SD

data JsonLocations a = JsonLocations { jsonDir :: Path.Path a Path.Dir, jsonUrl :: Text -> Text}

addJSON :: K.KnitEffects r
          => JsonLocations a
          -> Text
          -> A.Value
          -> K.Sem r Text
addJSON jl jsonName jsonVal = do
  let destDir = jsonDir jl
  K.liftKnit $ SD.createDirectoryIfMissing True (Path.toFilePath destDir)
  let jsonFileName = jsonName <> ".json"
      parseRel = first show . Path.parseRelFile . toString
  jsonPath' <- K.knitEither $ ((destDir Path.</>) <$> parseRel jsonFileName)
  K.liftKnit $ A.encodeFile (Path.toFilePath jsonPath') jsonVal
  pure $ jsonUrl jl jsonFileName
