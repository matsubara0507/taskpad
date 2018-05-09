{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TaskPad.Data.Monad
    ( TaskPad
    , Env
    , getFilepath
    ) where

import           RIO
import qualified RIO.Text            as Text
import           RIO.Time

import           Data.Extensible
import           TaskPad.Data.Config (Config, pathFormat)
import           TaskPad.Data.Memo   (Date)

type TaskPad = RIO Env

type Env = Record
  '[ "date"   >: Date
   , "config" >: Config
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)

getFilepath :: TaskPad FilePath
getFilepath = do
  date <- Text.unpack <$> asks (view #date)
  format <- Text.unpack <$> asks (view pathFormat . view #config)
  let path = reFormatTime defaultTimeLocale format date
  pure $ path <> ".yaml"

reFormatTime :: TimeLocale -> String -> String -> String
reFormatTime locale format =
  formatTime @ZonedTime locale format . parseTimeOrError True locale "%0Y%m%d"
