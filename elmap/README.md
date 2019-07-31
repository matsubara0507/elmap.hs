# elmap

This package is mapping to Elm type from Haskell type (thin-wrapper around [elm-bridge](http://hackage.haskell.org/package/elm-bridge)).

```Haskell
data Book = Book
    { title  :: String
    , author :: String
    , pages  :: Int
    } deriving (Generic, Show)

instance IsElmType Book where
  compileElmType _ = ETyCon $ ETCon "Book"

instance IsElmDefinition Book where
  compileElmDef _ = ETypeAlias $ EAlias
    { ea_name = ETypeName "Book" []
    , ea_fields =
        [ ("title", compileElmType (Proxy @ String))
        , ("author", compileElmType (Proxy @ String))
        , ("pages", compileElmType (Proxy @ Int))
        ]
    , ea_omit_null = False
    , ea_newtype = False
    , ea_unwrap_unary = True
    }
```

using ghci

```
>>> putStrLn $ renderElm $ compileElmType (Proxy @ Book)
Book
>>> putStrLn $ renderElm $ compileElmDef (Proxy @ Book)
type alias Book  =
   { title: String
   , author: String
   , pages: Int
   }
```
