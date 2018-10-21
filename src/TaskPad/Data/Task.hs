{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module TaskPad.Data.Task where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.List         as L

import           Data.Extensible
import qualified Data.Yaml        as Y
import           TaskPad.Data.Env (TaskPad)

type Task = Record
   '[ "id"   >: Int
    , "name" >: Text
    , "memo" >: Text
    , "done" >: Bool
    , "node" >: [Node]
    ]

type Node = Record
   '[ "id"      >: Int
    , "comment" >: Text
    , "time"    >: Int
    ]

mkTask :: Text -> TaskPad Task
mkTask name = do
  latestId <- getLatestId
  pure $ #id   @= latestId + 1
      <: #name @= name
      <: #memo @= ""
      <: #done @= False
      <: #node @= []
      <: nil

getLatestId :: TaskPad Int
getLatestId = do
  workDir <- asks (view #work . view #config)
  createDirectoryIfMissing True workDir
  files   <- map dropExtensions <$> listDirectory workDir
  pure . fromMaybe 0 $ L.maximumMaybe (catMaybes $ map readMaybe files)

addNode :: Task -> Text -> Int -> Task
addNode task comment time = task & #node `over` (node :)
  where
    node = #id      @= length (task ^. #node)
        <: #comment @= comment
        <: #time    @= time
        <: nil

writeTask :: Task -> TaskPad ()
writeTask task = do
  workDir <- asks (view #work . view #config)
  let path = workDir ++ "/" ++ show (task ^. #id) ++ ".yaml"
  logDebug (fromString $ "write task file: " <> path)
  writeTask' path task

writeTask' :: MonadIO m => FilePath -> Task -> m ()
writeTask' path task = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFileBinary path (Y.encode task)

readTask :: Int -> TaskPad Task
readTask idx = do
  workDir <- asks (view #work . view #config)
  let path = workDir ++ "/" ++ show idx ++ ".yaml"
  logDebug (fromString $ "read task file: " <> path)
  readTask' path

readTask' :: (MonadThrow m, MonadIO m) => FilePath -> m Task
readTask' path = either throwM pure =<< Y.decodeEither' <$> readFileBinary path

updateTask :: Int -> (Task -> Task) -> TaskPad Task
updateTask idx f = do
  workDir <- asks (view #work . view #config)
  let path = workDir ++ "/" ++ show idx ++ ".yaml"
  logDebug (fromString $ "update task file: " <> path)
  task <- f <$> readTask' path
  writeTask' path task
  pure task
