module Picshare04 exposing (main, photoDecoder)

import Browser
import Html exposing (Html, button, div, form, h1, h2, i, img, input, li, strong, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, fetchFeed )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


type alias Id =
    Int


type alias Photo =
    { id : Id
    , url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }


type alias Feed =
    List Photo


type alias Model =
    { feed : Maybe Feed
    , error : Maybe Http.Error
    }


initialModel : Model
initialModel =
    { feed = Nothing
    , error = Nothing
    }


photoDecoder : Json.Decode.Decoder Photo
photoDecoder =
    succeed Photo
        |> required "id" int
        |> required "url" string
        |> required "caption" string
        |> required "liked" bool
        |> required "comments" (list string)
        |> hardcoded ""


type Msg
    = ToggleLike Id
    | UpdateComment Id String
    | SaveComment Id
    | LoadFeed (Result Http.Error Feed)


toggleLike : Photo -> Photo
toggleLike photo =
    { photo | liked = not photo.liked }


updateComment : String -> Photo -> Photo
updateComment comment photo =
    { photo | newComment = comment }


saveNewComment : Photo -> Photo
saveNewComment photo =
    let
        comment =
            String.trim photo.newComment
    in
    case comment of
        "" ->
            photo

        _ ->
            { photo
                | comments = photo.comments ++ [ comment ]
                , newComment = ""
            }


updatePhotoById : (Photo -> Photo) -> Id -> Feed -> Feed
updatePhotoById updatePhoto id feed =
    List.map
        (\photo ->
            if photo.id == id then
                updatePhoto photo

            else
                photo
        )
        feed


updateFeed : (Photo -> Photo) -> Id -> Maybe Feed -> Maybe Feed
updateFeed updatePhoto id maybeFeed =
    Maybe.map (updatePhotoById updatePhoto id) maybeFeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike id ->
            ( { model | feed = updateFeed toggleLike id model.feed }
            , Cmd.none
            )

        UpdateComment id comment ->
            ( { model | feed = updateFeed (updateComment comment) id model.feed }
            , Cmd.none
            )

        SaveComment id ->
            ( { model | feed = updateFeed saveNewComment id model.feed }
            , Cmd.none
            )

        LoadFeed (Ok feed) ->
            ( { model | feed = Just feed }
            , Cmd.none
            )

        LoadFeed (Err error) ->
            ( { model | error = Just error }, Cmd.none )


viewCommentList : List String -> Html msg
viewCommentList comments =
    case comments of
        [] ->
            text ""

        _ ->
            div
                [ class "comments"
                ]
                [ ul
                    []
                    (List.map viewComment comments)
                ]


viewComment : String -> Html msg
viewComment comment =
    li []
        [ strong [] [ text "Comment:" ]
        , text (" " ++ comment)
        ]


viewComments : Photo -> Html Msg
viewComments photo =
    div []
        [ viewCommentList photo.comments
        , form
            [ class "new-comment"
            , onSubmit (SaveComment photo.id)
            ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , onInput (UpdateComment photo.id)
                , value photo.newComment
                ]
                []
            , button
                [ disabled (String.isEmpty photo.newComment) ]
                [ text "Save"
                ]
            ]
        ]


viewLoveButton : Photo -> Html Msg
viewLoveButton photo =
    let
        buttonClass =
            if photo.liked then
                "fa-heart"

            else
                "fa-heart-o"
    in
    div
        [ class "love-button"
        ]
        [ i
            [ class "fa fa-2x"
            , class buttonClass
            , onClick (ToggleLike photo.id)
            ]
            []
        ]


viewDetailedPhoto : Photo -> Html Msg
viewDetailedPhoto photo =
    div [ class "detailed-photo" ]
        [ img
            [ src photo.url
            ]
            []
        , div
            [ class "photo-info"
            ]
            [ viewLoveButton photo
            , h2
                [ class "caption"
                ]
                [ text photo.caption ]
            , viewComments photo
            ]
        ]


viewFeed : Maybe Feed -> Html Msg
viewFeed maybeFeed =
    case maybeFeed of
        Just feed ->
            div [] (List.map viewDetailedPhoto feed)

        Nothing ->
            div
                [ class "loading-feed"
                ]
                [ text "Loading feed..."
                ]


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadBody _ ->
            "Sorry, we couldn't process your feed at this time. We're working on it!"

        _ ->
            "Sorry, we couldn't load your feed at this time. Please try again later."


viewContent : Model -> Html Msg
viewContent model =
    case model.error of
        Just error ->
            div [ class "feed-error" ] [ text (errorMessage error) ]

        Nothing ->
            viewFeed model.feed


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ]
            ]
        , div [ class "content-flow" ]
            [ viewContent model
            ]
        ]


fetchFeed : Cmd Msg
fetchFeed =
    Http.get
        { url = baseUrl ++ "feed"
        , expect = Http.expectJson LoadFeed (list photoDecoder)
        }
