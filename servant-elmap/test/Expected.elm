getOne : (Result Http.Error  (Int)  -> msg) -> Cmd msg
getOne toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "one"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.int
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postTwo : String -> (Result Http.Error  ((Maybe Int))  -> msg) -> Cmd msg
postTwo body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "two"
                    ]
                    params
            , body =
                Http.jsonBody (Json.Encode.string body)
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (Json.Decode.int))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getBooksById : Int -> (Result Http.Error  (Book)  -> msg) -> Cmd msg
getBooksById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "books"
                    , capture_id |> String.fromInt
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecBook
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getBooksByTitle : String -> (Result Http.Error  (Book)  -> msg) -> Cmd msg
getBooksByTitle capture_title toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "books"
                    , capture_title
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecBook
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getBooks : Bool -> (Maybe String) -> (Maybe Int) -> String -> (List (Maybe Bool)) -> (Result Http.Error  ((List Book))  -> msg) -> Cmd msg
getBooks query_published query_sort query_year query_category query_filters toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ if query_published then
                    Just (Url.Builder.string "published" "")
                  else
                    Nothing ]
                , [ query_sort
                    |> Maybe.map (Url.Builder.string "sort") ]
                , [ query_year
                    |> Maybe.map (String.fromInt >> Url.Builder.string "year") ]
                , [ Just query_category
                    |> Maybe.map (Url.Builder.string "category") ]
                , query_filters
                    |> List.map (\val -> Just (Url.Builder.string "filters[]" (maybeBoolToIntStr val)))
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "books"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecBook))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postBooks : Book -> (Result Http.Error  (())  -> msg) -> Cmd msg
postBooks body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "books"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncBook body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getNothing : (Result Http.Error  (())  -> msg) -> Cmd msg
getNothing toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "nothing"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putNothing : (Result Http.Error  (())  -> msg) -> Cmd msg
putNothing toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "nothing"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getWithaheader : (Maybe String) -> (Maybe Int) -> String -> Int -> (Result Http.Error  (String)  -> msg) -> Cmd msg
getWithaheader header_myStringHeader header_MyIntHeader header_MyRequiredStringHeader header_MyRequiredIntHeader toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "myStringHeader") header_myStringHeader
                    , Maybe.map (Http.header "MyIntHeader" << String.fromInt) header_MyIntHeader
                    , Maybe.map (Http.header "MyRequiredStringHeader") (Just header_MyRequiredStringHeader)
                    , Maybe.map (Http.header "MyRequiredIntHeader" << String.fromInt) (Just header_MyRequiredIntHeader)
                    ]
            , url =
                Url.Builder.crossOrigin ""
                    [ "with-a-header"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getWitharesponseheader : (Result Http.Error  (String)  -> msg) -> Cmd msg
getWitharesponseheader toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "with-a-response-header"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
