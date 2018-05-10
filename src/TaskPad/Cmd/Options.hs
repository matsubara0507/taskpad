{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TaskPad.Cmd.Options where

import           RIO

import           Data.Extensible
import           TaskPad.Data.Memo (Date)

type Options = Record
  '[ "verbose" >: Bool
   , "date"    >: Maybe Date
   , "config"  >: FilePath
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "new"      >: ()
   , "add"      >: Text
   , "done"     >: Int
   , "tasks"    >: ()
   , "template" >: ()
   ]
