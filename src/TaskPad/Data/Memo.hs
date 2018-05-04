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
import qualified RIO.Text          as Text
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

readMemo :: (MonadIO m, MonadThrow m) => Date -> m Memo
readMemo date = do
  file <- readFileBinary (Text.unpack $ date <> ".yaml")
  either throwM pure $ Y.decodeEither' file

readMemoWithLog ::
  (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env) => Date -> m Memo
readMemoWithLog date = do
  memo <- readMemo date
  logDebug (display $ "read memo file: " <> date <> ".yaml")
  pure memo

writeMemo :: MonadIO m => Memo -> m ()
writeMemo memo =
  writeFileBinary (Text.unpack $ memo ^. #date <> ".yaml") (Y.encode memo)

writeMemoWithLog ::
  (MonadIO m, MonadReader env m, HasLogFunc env) => Memo -> m ()
writeMemoWithLog memo = do
  writeMemo memo
  logDebug (display $ "write memo file: " <> memo ^. #date <> ".yaml")
