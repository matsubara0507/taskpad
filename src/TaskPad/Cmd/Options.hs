{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TaskPad.Cmd.Options where

import           RIO

import           Data.Extensible

type Options = Record
  '[ "verbose" >: Bool
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant
  '[ "add"  >: Text
   , "done" >: Int
   ]
