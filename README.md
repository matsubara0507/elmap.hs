# elmap.hs

This project is mapping to Elm definitions from Haskell definitions.

- elmap package : Mapping to Elm Types from Haskell Type (thin-wrpper around [elm-bridge](http://hackage.haskell.org/package/elm-bridge))
- servant-elmap : Mapping Elm API client functions from Servant API (thin-wrapper around [servant-elm](http://hackage.haskell.org/package/servant-elm))
- extensible-elmap : Mapping to Elm Record Type from Extensible Record with [extensible](http://hackage.haskell.org/package/extensible) package

## Why not use elm-bridge/servant-elm

elm-bridge/servant-elm packages use `Typeable` type class to map Elm type from Haskell type.
But, does not work well if Haskell type is extensible's record.
Because, extensible's record type use type declaration:

```haskell
type User = Record
  '[ "id"    >: Int
   , "name"  >: String
   , "admin" >: Bool
   ]
```

## Example

see [example](example) project (Simple Todo Web Aplication):

```Haskell
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
```

Generate to [Elm code](example/elm-src/Generated/API.elm).

## Usage

Haskell Stack

```yaml
# add stack.yaml
extra-deps:
- github: matsubara0507/elmap.hs
  commit: xxx
  subdirs:
  - elmap
  - servant-elmap
  - extensible-elmap
```
