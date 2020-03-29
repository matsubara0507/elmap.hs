{-# LANGUAGE DeriveGeneric #-}

module Test where

import           Data.Aeson          (ToJSON (..), defaultOptions,
                                      genericToEncoding)
import           Data.Text           (Text)
import           GHC.Generics
import           Servant.API
import           Servant.Elm.Mapping

newtype Book = Book
    { title :: String
    } deriving (Generic, Show)

instance ToJSON Book where
  toEncoding = genericToEncoding defaultOptions

instance IsElmType Book where
  compileElmType _ = ETyCon $ ETCon "Book"

instance IsElmDefinition Book where
  compileElmDef = ETypeAlias . toElmAlias

type API =
       "one"
         :> Get '[JSON] Int
  :<|> "two"
         :> ReqBody '[JSON] String
         :> Post '[JSON] (Maybe Int)
  :<|> "books"
         :> Capture "id" Int
         :> Get '[JSON] Book
  :<|> "books"
         :> Capture "title" Text
         :> Get '[JSON] Book
  :<|> "books"
         :> QueryFlag "published"
         :> QueryParam "sort" String
         :> QueryParam "year" Int
         :> QueryParam' '[Required] "category" String
         :> QueryParams "filters" (Maybe Bool)
         :> Get '[JSON] [Book]
  :<|> "books"
         :> ReqBody '[JSON] Book
         :> PostNoContent '[JSON] ()
  :<|> "nothing"
         :> GetNoContent '[JSON] ()
  :<|> "nothing"
         :> Put '[JSON] () -- old way to specify no content
  :<|> "with-a-header"
         :> Header "Cookie" String
         :> Header "myStringHeader" String
         :> Header "MyIntHeader" Int
         :> Header' '[Required] "MyRequiredStringHeader" String
         :> Header' '[Required] "MyRequiredIntHeader" Int
         :> Get '[JSON] String
  :<|> "with-a-response-header"
         :> Get '[JSON] (Headers '[Header "myResponse" String] String)

api :: Proxy API
api = Proxy
