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

brAddJSON :: K.KnitEffects r
          => Path.Path a Path.Dir
          -> Text
          -> A.Value
          -> K.Sem r ()
brAddJSON destDir jsonName jsonVal = do
  K.liftKnit $ SD.createDirectoryIfMissing True (Path.toFilePath destDir)
  let jsonFileName = jsonName <> ".json"
      parseRel = first show . Path.parseRelFile . toString
  jsonPath' <- K.knitEither $ ((destDir Path.</>) <$> parseRel jsonFileName)
  K.liftKnit $ A.encodeFile (Path.toFilePath jsonPath') jsonVal


data JsonLocations a = JsonLocations { jsonDir :: Path.Path a Path.Dir, jsonUrl :: Text}
