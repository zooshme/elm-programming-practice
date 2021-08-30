module Picshare02 exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, i, img, input, li, strong, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


type alias Model =
    { url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }


initialModel : Model
initialModel =
    { url = baseUrl ++ "1.jpg"
    , caption = "Surfing"
    , liked = False
    , comments = [ "Cowabunda, dude!" ]
    , newComment = ""
    }


type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment


viewDetailedPhoto : Model -> Html Msg
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


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ]
            ]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto model
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLike ->
            { model | liked = not model.liked }

        UpdateComment comment ->
            { model | newComment = comment }

        SaveComment ->
            saveNewComment model


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


viewComments : Model -> Html Msg
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


saveNewComment : Model -> Model
saveNewComment model =
    let
        comment =
            String.trim model.newComment
    in
    case comment of
        "" ->
            model

        _ ->
            { model
                | comments = model.comments ++ [ comment ]
                , newComment = ""
            }
