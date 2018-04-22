{-# LANGUAGE OverloadedLabels #-}

module Test.TaskPad.Data.Task where

import           RIO

import           Data.Extensible
import           TaskPad.Data.Task
import           Test.Tasty
import           Test.Tasty.HUnit

task1 :: Task
task1
   = #name @= "Study Haskell."
  <: #done @= False
  <: #children @= []
  <: nil

subtask1 :: SubTask
subtask1
   = #name @= "Read \"Programming in Haskell\""
  <: #done @= False
  <: nil

subtask2 :: SubTask
subtask2
   = #name @= "Programming with Haskell"
  <: #done @= True
  <: nil

test_doneTask :: [TestTree]
test_doneTask =
  [ testCase "done task of Task type" $
      done task1 ^. #done @?= True
  , testCase "done task of SubTask type" $
      done subtask1 ^. #done @?= True
  , testCase "done task of SubTask type already done" $
      done subtask1 ^. #done @?= True
  ]
