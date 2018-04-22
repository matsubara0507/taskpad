{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           RIO

someFunc :: IO ()
someFunc = hPutBuilder stdout "someFunc"
