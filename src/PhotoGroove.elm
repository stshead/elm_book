module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)

photoListUrl : String
photoListUrl =
  "http://elm-in-action.com/list-photos"

urlPrefix : String
urlPrefix =
  "http://elm-in-action.com/"

type alias Msg =
  { description : String, data : String}

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
          [ onClick { description = "ClickedSurpriseMe", data = ""}]
          [ text "Suprise Me!"]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
          [ class "large"
          , src (urlPrefix ++ "large/" ++ model.selectedUrl)
          ]
          []
        ]

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
  img
    [ src (urlPrefix ++ thumb.url)
    , classList [("selected", selectedUrl == thumb.url)]
    , onClick { description = "ClickedPhoto", data = thumb.url}
    ]
    []

type alias Photo =
  { url : String }

type alias Model =
  { photos : List Photo
  , selectedUrl : String
  }

initialModel : Model
initialModel =
  { photos =
    [ { url = "1.jpeg" }
    , { url = "2.jpeg" }
    , { url = "3.jpeg" }
    ]
  , selectedUrl = "1.jpeg"
  }

photoArray : Array Photo
photoArray =
  Array.fromList initialModel.photos

update : Msg -> Model -> Model
update msg model =
  case msg.description of
    "ClickedPhoto" ->
      { model | selectedUrl = msg.data }
    "ClickedSurpriseMe" ->
      { model | selectedUrl = "2.jpeg" }
    _ ->
      model

main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
