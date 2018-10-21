{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TaskPad.Data.Env where

import           RIO

import           Data.Extensible
import           TaskPad.Data.Config (Config)

type TaskPad = RIO Env

type Env = Record
  '[ "config" >: Config
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
