{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TaskPad.Cmd.Run where

import           RIO
import qualified RIO.Map               as Map

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import           TaskPad.Cmd.Options
import           TaskPad.Data.Memo
import           TaskPad.Data.Monad
import           TaskPad.Data.Task

run :: MonadUnliftIO m => Options -> m ()
run opts = do
  date    <- maybe getTodaysDate pure $ opts ^. #date
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #date   @= date
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
    writeMemoWithLog $ mkMemo date
    logInfo (display $ "create new task's file: " <> date <> ".yaml")

instance Run ("add" >: Text) where
  run' _ txt = do
    date <- asks (view #date)
    memo <- readMemoWithLog date
    let key = foldr max 0 (Map.keys $ memo ^. #tasks) + 1
    writeMemoWithLog (memo & #tasks `over` Map.insert key (mkTask txt))
    logInfo ("add task: " <> display key)

instance Run ("done" >: Int) where
  run' _ key = do
    date <- asks (view #date)
    memo <- readMemoWithLog date
    writeMemoWithLog (memo & #tasks `over` Map.adjust done key)
    if Map.member key (memo ^. #tasks) then
      logInfo ("done task: " <> display key)
    else
      logError ("not found task: " <> display key)

instance Run ("tasks" >: ()) where
  run' _ _ = do
    date <- asks (view #date)
    memo <- readMemoWithLog date
    forM_ (Map.toList $ memo ^. #tasks) $ \(key, task) ->
      hPutBuilder stdout . encodeUtf8Builder $ mconcat
        [ tshow key, ": "
        , "[", if task ^. #done then "x" else " ", "] "
        , task ^. #name
        , "\n"
        ]
