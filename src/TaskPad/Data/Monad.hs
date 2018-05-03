{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module TaskPad.Data.Monad
    ( TaskPad
    , Env
    ) where

import           RIO

import           Data.Extensible
import           TaskPad.Data.Memo (Date)

type TaskPad = RIO Env

type Env = Record
  '[ "date"   >: Date
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
