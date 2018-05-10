{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import           RIO                        hiding (lift)

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import           GHC.TypeLits
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax

instance (Forall (KeyValue KnownSymbol (Instance1 Lift h)) xs) => Lift (RecordOf h xs) where
  lift =
    liftRecord . hfoldlWithIndexFor (Proxy :: Proxy (KeyValue KnownSymbol (Instance1 Lift h)))
      (\m xs x -> [| itemAssoc $(liftSymbolProxy $ symbolVal $ proxyAssocKey m) @:> $(lift $ getField x) |] : xs)
      mempty

deriving instance Lift (h (AssocValue kv)) => Lift (Field h kv)
deriving instance Lift a => Lift (Identity a)

liftSymbolProxy :: String -> Q Exp
liftSymbolProxy s = returnQ $ AppTypeE (ConE 'Proxy) (LitT (StrTyLit s))

liftRecord :: [Q Exp] -> Q Exp
liftRecord = foldl' (\xs x -> [| $x <: $xs |]) [|nil|]
