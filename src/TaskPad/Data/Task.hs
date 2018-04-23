{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module TaskPad.Data.Task
    ( Task
    , SubTask
    , HasTaskFields
    , TimeSchedule
    , Time
    , mkTask
    , mkSubTask
    , done
    , uncheck
    , addSubTask
    ) where

import           RIO

import           Data.Extensible

type Task = Record (TaskFields ++ '["children" >: [SubTask]])
type SubTask = Record TaskFields

type TaskFields =
  '[ "name" >: Text
   , "done" >: Bool
   ]

type HasTaskFields xs =
  ( Associate "name" Text xs
  , Associate "done" Bool xs
  )

type TimeSchedule = Record
  '[ "from" >: Time
   , "to"   >: Time
   , "task" >: Task
   ]

type Time = Text

mkTask :: Text -> Task
mkTask name = shrinkAssoc $ #children @= [] <: mkSubTask name

mkSubTask :: Text -> SubTask
mkSubTask name = #name @= name <: #done @= False <: nil

done, uncheck :: HasTaskFields xs => Record xs -> Record xs
done = #done `set` True
uncheck = #done `set` False

addSubTask :: SubTask -> Task -> Task
addSubTask task = #children `over` (task :)
