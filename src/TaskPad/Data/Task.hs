{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module TaskPad.Data.Task
    ( Task
    , SubTask
    , HasTaskFields
    , TimeSchedule
    , Time
    , done
    ) where

import           RIO

import           Data.Extensible

type Task = Record
  '[ "name"     >: Text
   , "done"     >: Bool
   , "children" >: [SubTask]
   ]

type SubTask = Record
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

done :: HasTaskFields xs => Record xs -> Record xs
done = #done `set` True
