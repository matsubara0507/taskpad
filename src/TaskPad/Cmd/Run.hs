{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TaskPad.Cmd.Run where

import           RIO
import qualified RIO.ByteString      as B

import           Data.Extensible
import           Data.Proxy
import qualified Data.Yaml           as Y
import           TaskPad.Cmd.Options
import           TaskPad.Data.Config
import           TaskPad.Data.Env
import           TaskPad.Data.Task

run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  config  <- readConfig (opts ^. #config)
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #config @= config
           <: #logger @= logger
           <: nil
    runRIO env $ matchField
      (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
      (opts ^. #subcmd)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

class Run kv where
  run' :: proxy kv -> AssocValue kv -> TaskPad ()

instance Run ("new" >: Text) where
  run' _ name = writeTask =<< mkTask name

instance Run ("update" >: (Int, Text)) where
  run' _ (idx, name) = do
    _ <- updateTask idx (#name `set` name)
    pure ()

instance Run ("done" >: Int) where
  run' _ idx = do
    _ <- updateTask idx (#done `set` True)
    pure ()

instance Run ("tasks" >: ()) where
  run' _ _ = showNotImpl

instance Run ("template" >: ()) where
  run' _ _ = B.putStr (Y.encode $ defaultConfig)
