{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elm.Mapping
  ( module X
  , IsElmType (..)
  , toElmTypeWith
  , renameEType
  , Proxy (..)
  ) where

import           Data.Functor.Identity (Identity (..))
import           Data.Map              (Map)
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Typeable         (Typeable)
import           Elm.TyRender          as X
import           Elm.TyRep             as X

class IsElmType a where
  compileElmType :: Proxy a -> EType

instance IsElmType Int      where compileElmType _ = toElmType (Proxy @ Int)
instance IsElmType Integer  where compileElmType _ = toElmType (Proxy @ Int)
instance IsElmType Float    where compileElmType _ = toElmType (Proxy @ Float)
instance IsElmType Double   where compileElmType _ = toElmType (Proxy @ Float)
instance IsElmType Ordering where compileElmType _ = toElmTypeWith "Order" (Proxy @ Ordering)
instance IsElmType Bool     where compileElmType _ = toElmType (Proxy @ Bool)
instance IsElmType Char     where compileElmType _ = toElmType (Proxy @ Char)
instance IsElmType String   where compileElmType _ = toElmType (Proxy @ String)
instance IsElmType Text     where compileElmType _ = toElmType (Proxy @ String)
instance IsElmType ()       where compileElmType _ = toElmType (Proxy @ ())

instance {-# OVERLAPPABLE #-} IsElmType a => IsElmType [a] where
  compileElmType _ = ETyApp (ETyCon $ ETCon "List") (compileElmType (Proxy @ a))

instance IsElmType a => IsElmType (Maybe a) where
  compileElmType _ = ETyApp (ETyCon $ ETCon "Maybe") (compileElmType (Proxy @ a))

instance (IsElmType k, IsElmType v) => IsElmType (Map k v) where
  compileElmType _ =
    foldl ETyApp (ETyCon $ ETCon "Dict")
      [ compileElmType (Proxy @ k)
      , compileElmType (Proxy @ v)
      ]

instance (IsElmType a, IsElmType b) => IsElmType (a, b) where
  compileElmType _ =
    foldl ETyApp (ETyTuple 2)
      [ compileElmType (Proxy @ a)
      , compileElmType (Proxy @ b)
      ]

instance (IsElmType a, IsElmType b, IsElmType c) => IsElmType (a, b, c) where
  compileElmType _ =
    foldl ETyApp (ETyTuple 3)
      [ compileElmType (Proxy @ a)
      , compileElmType (Proxy @ b)
      , compileElmType (Proxy @ c)
      ]

instance (IsElmType a, IsElmType b, IsElmType c, IsElmType d) => IsElmType (a, b, c, d) where
  compileElmType _ =
    foldl ETyApp (ETyTuple 4)
      [ compileElmType (Proxy @ a)
      , compileElmType (Proxy @ b)
      , compileElmType (Proxy @ c)
      , compileElmType (Proxy @ d)
      ]

instance IsElmType a => IsElmType (Identity a) where
  compileElmType _ = compileElmType (Proxy @ a)

toElmTypeWith :: (Typeable a) => String -> Proxy a -> EType
toElmTypeWith name = renameEType name . toElmType

renameEType :: String -> EType -> EType
renameEType name (ETyCon _)     = ETyCon $ ETCon name
renameEType name (ETyApp t1 t2) = ETyApp (renameEType name t1) t2
renameEType _ t                 = t
