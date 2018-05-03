{-# LANGUAGE OverloadedLabels #-}

module TaskPad.Cmd.Run where

import           RIO

import           TaskPad.Cmd.Options

run :: MonadIO m => Options -> m ()
run _ = hPutBuilder stdout "not yet implement command."
