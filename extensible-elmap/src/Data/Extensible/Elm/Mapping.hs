{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Extensible.Elm.Mapping
  ( compileElmRecordTypeWith
  , compileElmRecordAliasWith
  ) where

import           Data.Extensible
import           Elm.Mapping
import           GHC.TypeLits    (KnownSymbol, symbolVal)

compileElmRecordTypeWith :: String -> Proxy (Record xs) -> EType
compileElmRecordTypeWith name _ = ETyCon $ ETCon name

compileElmRecordAliasWith ::
  forall xs . Forall (KeyTargetAre KnownSymbol IsElmType) xs
  => String
  -> Proxy (Record xs)
  -> EAlias
compileElmRecordAliasWith name _ = EAlias
  { ea_name = ETypeName name []
  , ea_fields = fields
  , ea_omit_null = False
  , ea_newtype = False
  , ea_unwrap_unary = True
  }
  where
    fields = henumerateFor
      (Proxy @ (KeyTargetAre KnownSymbol IsElmType))
      (Proxy @ xs)
      (\m acc -> (stringKeyOf m, compileElmType $ proxyTargetOf m) : acc)
      []
