module Psx.Format.Config where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:?), printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.String as String
import Effect (Effect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path as Path
import Tidy (ImportSortOption(..), ImportWrapOption(..), TypeArrowOption(..), UnicodeOption(..))
import Tidy.Operators.Defaults (defaultOperators)

type FormatOptions =
  { importSort :: ImportSortOption
  , importWrap :: ImportWrapOption
  , indent :: Int
  , operatorsFile :: Maybe String
  , ribbon :: Number
  , typeArrowPlacement :: TypeArrowOption
  , unicode :: UnicodeOption
  , width :: Maybe Int
  }

defaults :: FormatOptions
defaults =
  { importSort: ImportSortSource
  , importWrap: ImportWrapSource
  , indent: 2
  , operatorsFile: Nothing
  , ribbon: 1.0
  , typeArrowPlacement: TypeArrowFirst
  , unicode: UnicodeSource
  , width: Nothing
  }

fromJson :: Json -> Either JsonDecodeError FormatOptions
fromJson json = do
  obj <- decodeJson json
  importSort <- traverse importSortFromString =<< obj .:? "importSort"
  importWrap <- traverse importWrapFromString =<< obj .:? "importWrap"
  indent <- obj .:? "indent"
  operatorsFile <- obj .:? "operatorsFile"
  ribbon <- obj .:? "ribbon"
  typeArrowPlacement <- traverse typeArrowPlacementFromString =<< obj .:? "typeArrowPlacement"
  unicode <- traverse unicodeFromString =<< obj .:? "unicode"
  width <- obj .:? "width"
  pure
    { importSort: fromMaybe defaults.importSort importSort
    , importWrap: fromMaybe defaults.importWrap importWrap
    , indent: fromMaybe defaults.indent indent
    , operatorsFile: operatorsFile <|> defaults.operatorsFile
    , ribbon: fromMaybe defaults.ribbon ribbon
    , typeArrowPlacement: fromMaybe defaults.typeArrowPlacement typeArrowPlacement
    , unicode: fromMaybe defaults.unicode unicode
    , width: width <|> defaults.width
    }

typeArrowPlacementFromString :: String -> Either JsonDecodeError TypeArrowOption
typeArrowPlacementFromString = case _ of
  "first" -> pure TypeArrowFirst
  "last" -> pure TypeArrowLast
  other -> throwError $ UnexpectedValue (Json.fromString other)

unicodeFromString :: String -> Either JsonDecodeError UnicodeOption
unicodeFromString = case _ of
  "source" -> pure UnicodeSource
  "always" -> pure UnicodeAlways
  "never" -> pure UnicodeNever
  other -> throwError $ UnexpectedValue (Json.fromString other)

importWrapFromString :: String -> Either JsonDecodeError ImportWrapOption
importWrapFromString = case _ of
  "source" -> pure ImportWrapSource
  "auto" -> pure ImportWrapAuto
  other -> throwError $ UnexpectedValue (Json.fromString other)

importSortFromString :: String -> Either JsonDecodeError ImportSortOption
importSortFromString = case _ of
  "source" -> pure ImportSortSource
  "ide" -> pure ImportSortIde
  other -> throwError $ UnexpectedValue (Json.fromString other)

-- The nearest configuration wins; paths in it are relative to its directory.
loadConfig :: String -> Effect { settings :: FormatOptions, operators :: Array String }
loadConfig file = do
  absolute <- Path.resolve [] file
  go (Path.dirname absolute)
  where
  go dir = do
    let path = Path.concat [ dir, ".tidyrc.json" ]
    exists <- FS.exists path
    if exists then do
      text <- FS.readTextFile UTF8 path
      settings <- case jsonParser text of
        Left err -> throw (path <> ": " <> err)
        Right json -> case fromJson json of
          Left err -> throw (path <> ": " <> printJsonDecodeError err)
          Right value -> pure value
      operators <- case settings.operatorsFile of
        Nothing -> pure defaultOperators
        Just name -> do
          operatorPath <- Path.resolve [ dir ] name
          String.split (String.Pattern "\n") <$> FS.readTextFile UTF8 operatorPath
      pure { settings, operators }
    else if Path.dirname dir == dir then
      pure { settings: defaults { importSort = ImportSortIde, unicode = UnicodeNever }, operators: defaultOperators }
    else go (Path.dirname dir)
