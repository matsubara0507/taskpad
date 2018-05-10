{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TaskPad.Cmd.Run where

import           RIO
import qualified RIO.ByteString        as B
import qualified RIO.Map               as Map

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import qualified Data.Yaml             as Y
import           TaskPad.Cmd.Options
import           TaskPad.Data.Config
import           TaskPad.Data.Memo
import           TaskPad.Data.Monad
import           TaskPad.Data.Task

run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  date    <- maybe getTodaysDate pure $ opts ^. #date
  config  <- readConfig (opts ^. #config)
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #date   @= date
           <: #config @= config
           <: #logger @= logger
           <: nil
    runRIO env $ do
      when (isNothing $ opts ^. #date) $
        logDebug (display $ "get today's date: " <> date)
      matchField
        (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
        (opts ^. #subcmd)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

class Run kv where
  run' :: proxy kv -> AssocValue kv -> TaskPad ()

instance Run ("new" >: ()) where
  run' _ _ = do
    date <- asks (view #date)
    path <- getFilepath
    writeMemoWithLog path $ mkMemo date
    logInfo (fromString $ "create new task's file: " <> path)

instance Run ("add" >: Text) where
  run' _ txt = do
    path <- getFilepath
    memo <- readMemoWithLog path
    let key = foldr max 0 (Map.keys $ memo ^. #tasks) + 1
    writeMemoWithLog path (memo & #tasks `over` Map.insert key (mkTask txt))
    logInfo ("add task: " <> display key)

instance Run ("done" >: Int) where
  run' _ key = do
    path <- getFilepath
    memo <- readMemoWithLog path
    writeMemoWithLog path (memo & #tasks `over` Map.adjust done key)
    if Map.member key (memo ^. #tasks) then
      logInfo ("done task: " <> display key)
    else
      logError ("not found task: " <> display key)

instance Run ("tasks" >: ()) where
  run' _ _ = do
    path <- getFilepath
    memo <- readMemoWithLog path
    forM_ (Map.toList $ memo ^. #tasks) $ \(key, task) ->
      hPutBuilder stdout . encodeUtf8Builder $ mconcat
        [ tshow key, ": "
        , "[", if task ^. #done then "x" else " ", "] "
        , task ^. #name
        , "\n"
        ]

instance Run ("template" >: ()) where
  run' _ _ = B.putStr (Y.encode $ defaultConfig)
