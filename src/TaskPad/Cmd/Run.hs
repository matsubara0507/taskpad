{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module TaskPad.Cmd.Run where

import           RIO
import qualified RIO.Text              as Text

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import qualified Data.Yaml             as Y
import           TaskPad.Cmd.Options
import           TaskPad.Data.Memo     (getTodaysDate, mkMemo)

run :: MonadIO m => Options -> m ()
run opts = flip matchField (opts ^. #subcmd) $
  htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

class Run kv where
  run' :: MonadIO m => proxy kv -> AssocValue kv -> m ()

instance Run ("new" >: NewOption) where
  run' _ opts = do
    date <- maybe getTodaysDate pure $ opts ^. #date
    let memo = mkMemo date
    liftIO $ Y.encodeFile (Text.unpack $ date <> ".yaml") memo

instance Run ("add" >: Text) where
  run' _ _ = showNotImpl

instance Run ("done" >: Int) where
  run' _ _ = showNotImpl
