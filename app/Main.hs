{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Paths_taskpad       (version)
import           RIO

import           Data.Extensible
import           Data.Proxy
import           Data.Version        (Version)
import qualified Data.Version        as Version
import           Development.GitRev
import           GHC.TypeLits
import           Options.Applicative
import           TaskPad.Cmd

main :: IO ()
main = runWithShowVersion (showVersion version) =<< execParser opts
  where
    opts = info (options <**> helper)
         $ fullDesc
        <> header "taskpad - operate daily tasks"

options :: Parser Options
options = hsequence
    $ #version <@=> switch (long "version" <> help "Show version")
   <: #verbose <@=> switch (long "verbose" <> short 'v' <> help "Enable verbose mode: verbosity level \"debug\"")
   <: #subcmd  <@=> (fmap pure subcmdParser <|> pure Nothing)
   <: nil

subcmdParser :: Parser SubCmd
subcmdParser = variantFrom
    $ #add  @= (strArgument (metavar "TEXT" <> help "Add task") `withInfo` "Add Task")
   <: #done @= (argument auto (metavar "ID" <> help "Done task") `withInfo` "Done Task")
   <: nil

variantFrom ::
  Forall (KeyIs KnownSymbol) xs => RecordOf ParserInfo xs -> Parser (Variant xs)
variantFrom = subparser . subcmdVariant
  where
    subcmdVariant = hfoldMapWithIndexFor (Proxy @ (KeyIs KnownSymbol)) $ \m x ->
      let k = symbolVal (proxyAssocKey m)
      in command k ((EmbedAt m . Field . pure) <$> getField x)

instance Wrapper ParserInfo where
  type Repr ParserInfo a = ParserInfo a
  _Wrapper = id

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts = info (helper <*> opts) . progDesc

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
