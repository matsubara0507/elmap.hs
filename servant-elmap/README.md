# servant-elmap

This package is mapping Elm API client functions from Servant API (thin-wrapper around [servant-elm](http://hackage.haskell.org/package/servant-elm)).

```Haskell
type CRUD = "todos" :> Get '[JSON] [String]
       :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] String :> Post '[JSON] String
```

generate to:

```Elm
getApiTodos : (Result Http.Error (List String) -> msg) -> Cmd msg
getApiTodos toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "todos"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list Json.Decode.string)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postApiTodos : String -> (Result Http.Error String -> msg) -> Cmd msg
postApiTodos body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "todos"
                ]
                params
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectJson toMsg Json.Decode.string
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
```

## Requirement Elm packages

- `elm/http` >= 2.0.0
- `elm/json`
- `bartavelle/json-helpers`
