{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import           RIO                        hiding (lift)

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import           GHC.TypeLits
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax

instance (Forall (KeyValue KnownSymbol Lift) xs) => Lift (Record xs) where
  lift r =
    liftRecord $ hfoldlWithIndexFor
      (Proxy @ (KeyValue KnownSymbol Lift))
      (\m xs x ->
        let k = symbolVal $ proxyAssocKey m
        in [| itemAssoc $(liftSymbolProxy k) @= $(lift $ runIdentity (getField x)) |] : xs)
      []
      r

liftSymbolProxy :: String -> Q Exp
liftSymbolProxy s = returnQ $ AppTypeE (ConE 'Proxy) (LitT (StrTyLit s))

liftRecord :: [Q Exp] -> Q Exp
liftRecord = foldl' (\xs x -> [| $x <: $xs |]) [|nil|]
