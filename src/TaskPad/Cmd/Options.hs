{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TaskPad.Cmd.Options where

import           RIO

import           Data.Extensible
import           TaskPad.Data.Memo (Date)

type Options = Record
  '[ "verbose" >: Bool
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "new"  >: NewOption
   , "add"  >: Text
   , "done" >: Int
   ]

type NewOption = Record
  '[ "date" >: Maybe Date
   ]
