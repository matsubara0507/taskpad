{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module TaskPad.Data.Memo
    ( Memo
    , Date
    , mkMemo
    , getTodaysDate
    , readMemo
    , readMemoWithLog
    , writeMemo
    , writeMemoWithLog
    ) where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import           RIO.Time

import           Data.Extensible
import qualified Data.Yaml         as Y
import           TaskPad.Data.Task

type Memo = Record
  '[ "date"  >: Date
   , "tasks" >: Map Int Task
   , "memo"  >: [Text]
   ]

type Date = Text

mkMemo :: Date -> Memo
mkMemo date
    = #date  @= date
   <: #tasks @= mempty
   <: #memo  @= mempty
   <: nil

getTodaysDate :: MonadIO m => m Date
getTodaysDate =
  fromString . formatTime defaultTimeLocale "%0Y%m%d" <$> getZonedTime

readMemo :: (MonadIO m, MonadThrow m) => FilePath -> m Memo
readMemo path = do
  file <- readFileBinary path
  either throwM pure $ Y.decodeEither' file

readMemoWithLog ::
  (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env)
  => FilePath -> m Memo
readMemoWithLog path = do
  memo <- readMemo path
  logDebug (fromString $ "read memo file: " <> path)
  pure memo

writeMemo :: MonadIO m => FilePath -> Memo -> m ()
writeMemo path memo = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFileBinary path (Y.encode memo)

writeMemoWithLog ::
  (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> Memo -> m ()
writeMemoWithLog path memo = do
  writeMemo path memo
  logDebug (fromString $ "write memo file: " <> path)
