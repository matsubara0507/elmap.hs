# extensible-elmap

This package is mapping to Elm record type from Haskell record type with [extensible](http://hackage.haskell.org/package/extensible).

```Haskell
type Book = Record
  '[ "title"  >: String
   , "author" >: String
   , "pages"  >: Int
   ]

instance IsElmType User where
  compileElmType = compileElmRecordTypeWith "Book"

instance IsElmDefinition User where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Book"
```

using ghci:

```
>>> putStrLn $ renderElm $ compileElmType (Proxy @Book)
Book
>>> putStrLn $ renderElm $ compileElmDef (Proxy @Book)
type alias Book  =
   { title: String
   , author: String
   , pages: Int
   }
```
