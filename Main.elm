-- Press a button to send a GET request for random cat GIFs.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Data = 
  { name : String
  , url : String 
  }

type Model
  = Failure
  | Loading Data
  | Success Data


init : () -> (Model, Cmd Msg)
init _ =
  (Loading (Data "" ""), getRandomCatGif)



-- UPDATE


type Msg
  = MorePlease
  | GotGif (Result Http.Error String)
  | ChangeName String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      case model of
        Success data ->
          (Success data, getRandomCatGif)

        Loading data ->
          (Success data, getRandomCatGif)

        Failure ->
          (Failure, Cmd.none)


    GotGif result ->
      case result of
        Ok url ->
          case model of
            Success data ->
              (Success {data | url = url}, Cmd.none)

            Loading data ->
              (Success {data | url = url}, Cmd.none)

            Failure ->
              (Failure, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)
          
    ChangeName str ->
      case model of
        Success data ->
          (Success {data | name = str}, Cmd.none)
          
        Loading data ->
          (Loading {data | name = str}, Cmd.none)
          
        Failure ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Random Cats" ]
    , viewGif model
    ]


viewGif : Model -> Html Msg
viewGif model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random cat for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading data ->
      text "Loading..."

    Success data ->
      div []
        [ h1 [] [ text data.name ]
        , button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , img [ src data.url ] []
        , div [] [ input [ placeholder "name", onInput ChangeName ] [] ]
        ]



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
  Http.get
    { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
    , expect = Http.expectJson GotGif gifDecoder
    }


gifDecoder : Decoder String
gifDecoder =
  field "data" (field "image_url" string)
