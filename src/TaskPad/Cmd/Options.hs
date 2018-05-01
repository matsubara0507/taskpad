{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TaskPad.Cmd.Options where

import           RIO

import           Data.Extensible

type Options = Record
  '[ "version" >: Bool
   , "verbose" >: Bool
   , "subcmd"  >: Maybe SubCmd
   ]

type SubCmd = Variant
  '[ "add"  >: Text
   , "done" >: Int
   ]
