{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Elm.Mapping.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Elm.Mapping     (EType, IsElmType (..))
import           Servant.API     (Headers (..))
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)

data LangElmap

--- TODO: Generate Elm functions that can handle the response headers. PRs
--- welcome!
instance {-# OVERLAPPING #-} (IsElmType a) => HasForeignType LangElmap EType (Headers b a) where
  typeFor _ _ _ = compileElmType (Proxy @ a)

instance {-# OVERLAPPABLE #-} (IsElmType a) => HasForeignType LangElmap EType a where
  typeFor _ _ _ = compileElmType (Proxy @ a)

getEndpoints ::
  (HasForeign LangElmap EType api, GenerateList EType (Foreign EType api))
  => Proxy api
  -> [Req EType]
getEndpoints = listFromAPI (Proxy @ LangElmap) (Proxy @ EType)
