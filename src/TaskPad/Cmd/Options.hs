{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TaskPad.Cmd.Options where

import           RIO

import           Data.Extensible

type Options = Record
  '[ "verbose" >: Bool
   , "config"  >: FilePath
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "new"      >: Text
   , "update"   >: (Int, Text)
   , "done"     >: Int
   , "tasks"    >: ()
   , "template" >: ()
   ]
