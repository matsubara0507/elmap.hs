{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elm.Mapping
  ( module X
  , IsElmType (..)
  , toElmTypeWith
  , renameEType
  , toElmAlias
  , Proxy (..)
  , DefineElm (..)
  ) where

import           Data.Functor.Identity (Identity (..))
import           Data.Kind             (Type)
import           Data.Map              (Map)
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Data.Typeable         (Typeable)
import           Elm.Module            (DefineElm (..))
import           Elm.TyRender          as X
import           Elm.TyRep             as X
import           GHC.Generics

class IsElmType a where
  compileElmType :: Proxy a -> EType

instance IsElmType Int      where compileElmType _ = toElmType (Proxy @Int)
instance IsElmType Integer  where compileElmType _ = toElmType (Proxy @Int)
instance IsElmType Float    where compileElmType _ = toElmType (Proxy @Float)
instance IsElmType Double   where compileElmType _ = toElmType (Proxy @Float)
instance IsElmType Ordering where compileElmType _ = toElmTypeWith "Order" (Proxy @Ordering)
instance IsElmType Bool     where compileElmType _ = toElmType (Proxy @Bool)
instance IsElmType Char     where compileElmType _ = toElmType (Proxy @Char)
instance IsElmType String   where compileElmType _ = toElmType (Proxy @String)
instance IsElmType Text     where compileElmType _ = toElmType (Proxy @String)
instance IsElmType ()       where compileElmType _ = toElmType (Proxy @())

instance {-# OVERLAPPABLE #-} IsElmType a => IsElmType [a] where
  compileElmType _ = ETyApp (ETyCon $ ETCon "List") (compileElmType (Proxy @a))

instance IsElmType a => IsElmType (Maybe a) where
  compileElmType _ = ETyApp (ETyCon $ ETCon "Maybe") (compileElmType (Proxy @a))

instance (IsElmType k, IsElmType v) => IsElmType (Map k v) where
  compileElmType _ =
    foldl ETyApp (ETyCon $ ETCon "Dict")
      [ compileElmType (Proxy @k)
      , compileElmType (Proxy @v)
      ]

instance (IsElmType a, IsElmType b) => IsElmType (a, b) where
  compileElmType _ =
    foldl ETyApp (ETyTuple 2)
      [ compileElmType (Proxy @a)
      , compileElmType (Proxy @b)
      ]

instance (IsElmType a, IsElmType b, IsElmType c) => IsElmType (a, b, c) where
  compileElmType _ =
    foldl ETyApp (ETyTuple 3)
      [ compileElmType (Proxy @a)
      , compileElmType (Proxy @b)
      , compileElmType (Proxy @c)
      ]

instance (IsElmType a, IsElmType b, IsElmType c, IsElmType d) => IsElmType (a, b, c, d) where
  compileElmType _ =
    foldl ETyApp (ETyTuple 4)
      [ compileElmType (Proxy @a)
      , compileElmType (Proxy @b)
      , compileElmType (Proxy @c)
      , compileElmType (Proxy @d)
      ]

instance IsElmType a => IsElmType (Identity a) where
  compileElmType _ = compileElmType (Proxy @a)

toElmTypeWith :: Typeable a => String -> Proxy a -> EType
toElmTypeWith name = renameEType name . toElmType

renameEType :: String -> EType -> EType
renameEType name (ETyCon _)     = ETyCon $ ETCon name
renameEType name (ETyApp t1 t2) = ETyApp (renameEType name t1) t2
renameEType _ t                 = t

toElmAlias :: forall a. (GIsElmFields (Rep a), IsElmType a) => Proxy a -> EAlias
toElmAlias proxy = EAlias
  { ea_name = ETypeName (renderElm $ compileElmType proxy) []
  , ea_fields = gcompileElmFields (Proxy @(Rep a))
  , ea_omit_null = False
  , ea_newtype = False
  , ea_unwrap_unary = True
  }

class GIsElmFields (rep :: Type -> Type) where
  gcompileElmFields :: Proxy rep -> [(String, EType)]

instance GIsElmFields a => GIsElmFields (M1 D x a) where
  gcompileElmFields _ = gcompileElmFields (Proxy @a)

instance GIsElmFields a => GIsElmFields (M1 C x a) where
  gcompileElmFields _ = gcompileElmFields (Proxy @a)

instance (Selector x, IsElmType a) => GIsElmFields (M1 S x (K1 R a)) where
  gcompileElmFields _ = [(selName (undefined :: S1 x (K1 R a) ()), compileElmType (Proxy @a))]

instance (GIsElmFields a, GIsElmFields b) => GIsElmFields (a :*: b) where
  gcompileElmFields _ = gcompileElmFields (Proxy @a) ++ gcompileElmFields (Proxy @b)
