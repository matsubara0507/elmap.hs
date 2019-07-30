module Todo where

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Servant.API

type Todo = Record
  '[ "id" >: Int
   , "title" >: String
   , "done" >: Bool
   ]

instance IsElmType Todo where
  compileElmType = compileElmRecordTypeWith "Todo"

instance IsElmDefinition Todo where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Todo"

type CRUD = "todos" :> Get '[JSON] [Todo]
       :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
       :<|> "todos" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
       :<|> "todos" :> Capture "id" Int :> Delete '[JSON] ()

crud :: Proxy CRUD
crud = Proxy
