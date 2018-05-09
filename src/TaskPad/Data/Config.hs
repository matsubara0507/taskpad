{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module TaskPad.Data.Config where

import           RIO
import           RIO.Directory   (doesFileExist)

import           Data.Extensible
import           Data.Proxy
import qualified Data.Yaml       as Y

type Config = Record ConfigFields

type ConfigFields =
  '[ "root" >: Text
   , "path-format" >: Text
   ]

defaultConfig :: Config
defaultConfig
    = #root      @= "."
   <: pathFormat @= "%0Y%m%d"
   <: nil

readConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Config
readConfig = readConfigWith defaultConfig

readConfigWith :: (MonadIO m, MonadThrow m) => Config -> FilePath -> m Config
readConfigWith def path = do
  file <- readFileBinaryWith "" path
  if Y.decodeEither file == Right Y.Null then
    pure def
  else do
    config <- either throwM pure $ Y.decodeEither' file
    pure $ constructWith def config

readFileBinaryWith :: MonadIO m => ByteString -> FilePath -> m ByteString
readFileBinaryWith def path =
  doesFileExist path >>= bool (pure def) (readFileBinary path)

constructWith :: RecordOf h xs -> Nullable (Field h) :* xs -> RecordOf h xs
constructWith def =
  hmapWithIndex $ \m x -> fromMaybe (hlookup m def) (getNullable x)

pathFormat :: FieldOptic "path-format"
pathFormat = itemAssoc (Proxy @ "path-format")
