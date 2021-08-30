module Picshare03 exposing (main, photoDecoder)

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


type alias Model =
    { photo : Maybe Photo }


initialModel : Model
initialModel =
    { photo =
        Nothing
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
    = ToggleLike
    | UpdateComment String
    | SaveComment
    | LoadFeed (Result Http.Error Photo)


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


updateFeed : (Photo -> Photo) -> Maybe Photo -> Maybe Photo
updateFeed updatePhoto maybePhoto =
    Maybe.map updatePhoto maybePhoto


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike ->
            ( { model | photo = updateFeed toggleLike model.photo }
            , Cmd.none
            )

        UpdateComment comment ->
            ( { model | photo = updateFeed (updateComment comment) model.photo }
            , Cmd.none
            )

        SaveComment ->
            ( { model | photo = updateFeed saveNewComment model.photo }
            , Cmd.none
            )

        LoadFeed (Ok photo) ->
            ( { model | photo = Just photo }
            , Cmd.none
            )

        LoadFeed (Err _) ->
            ( model, Cmd.none )


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
viewComments model =
    div []
        [ viewCommentList model.comments
        , form
            [ class "new-comment"
            , onSubmit SaveComment
            ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , onInput UpdateComment
                , value model.newComment
                ]
                []
            , button
                [ disabled (String.isEmpty model.newComment) ]
                [ text "Save"
                ]
            ]
        ]


viewDetailedPhoto : Photo -> Html Msg
viewDetailedPhoto model =
    let
        buttonClass =
            if model.liked then
                "fa-heart"

            else
                "fa-heart-o"
    in
    div [ class "detailed-photo" ]
        [ img
            [ src model.url
            ]
            []
        , div
            [ class "photo-info"
            ]
            [ div
                [ class "like-button"
                ]
                [ i
                    [ class "fa fa-2x"
                    , class buttonClass
                    , onClick ToggleLike
                    ]
                    []
                ]
            , h2
                [ class "caption"
                ]
                [ text model.caption ]
            , viewComments model
            ]
        ]


viewFeed : Maybe Photo -> Html Msg
viewFeed maybePhoto =
    case maybePhoto of
        Just photo ->
            viewDetailedPhoto photo

        Nothing ->
            div
                [ class "loading-feed"
                ]
                [ text "Loading feed..."
                ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ]
            ]
        , div [ class "content-flow" ]
            [ viewFeed model.photo
            ]
        ]


fetchFeed : Cmd Msg
fetchFeed =
    Http.get
        { url = baseUrl ++ "feed/1"
        , expect = Http.expectJson LoadFeed photoDecoder
        }
