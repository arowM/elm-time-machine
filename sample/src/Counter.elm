module Counter exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MODEL


type alias Model =
  { counter : Int
  }


init : (Model, Cmd Msg)
init =
  ( { counter = 0
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Increment ->
      ( { model
        | counter = model.counter + 1
        }
      , Cmd.none
      )

    Decrement ->
      ( { model
        | counter = model.counter - 1
        }
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  div [class "container"]
    [ div [class "counter"]
      [ text <| toString model.counter
      ]
    , div [class "increment"]
      [ button
        [ type' "button"
        , onClick Increment
        ]
        [ text "+" ]
      ]
    , div [class "decrement"]
      [ button
        [ type' "button"
        , onClick Decrement
        ]
        [ text "-" ]
      ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
