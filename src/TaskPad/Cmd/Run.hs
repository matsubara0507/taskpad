{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TaskPad.Cmd.Run where

import           RIO
import qualified RIO.Text              as Text

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import qualified Data.Yaml             as Y
import           TaskPad.Cmd.Options
import           TaskPad.Data.Memo     (getTodaysDate, mkMemo)
import           TaskPad.Data.Monad

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
    let memo = mkMemo date
    liftIO $ Y.encodeFile (Text.unpack $ date <> ".yaml") memo
    logInfo (display $ "create new task's file: " <> date <> ".yaml")

instance Run ("add" >: Text) where
  run' _ _ = showNotImpl

instance Run ("done" >: Int) where
  run' _ _ = showNotImpl
