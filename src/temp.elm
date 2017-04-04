module Main exposing (..)

import Html exposing (..)


type alias Model =
    { url : String
    , title : String
    }


type alias Flags =
    { url : String
    }


initialModel : Model
initialModel =
    { url = ""
    , title = "This is the current Url"
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initialModel
        | url = flags.url
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ span []
            [ text (model.title) ]
        , span [] [ text (model.url) ]
        ]


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Example ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = Example
