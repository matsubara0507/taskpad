{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module TaskPad.Data.Memo
    ( Memo
    , Date
    , mkMemo
    , getTodaysDate
    ) where

import           RIO
import           RIO.Time

import           Data.Extensible
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
