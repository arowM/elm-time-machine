import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import TimeMachine exposing (TimeMachine)

import Counter



-- APP


main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { counter1 : TimeMachine Counter.Model
  , counter2 : Counter.Model
  }


init : (Model, Cmd Msg)
init =
  let
    (counter1Model, counter1Cmd) = Counter.init
    (counter2Model, counter2Cmd) = Counter.init
  in
    ( { counter1 = TimeMachine.succeed counter1Model
      , counter2 = counter2Model
      }
    , Cmd.batch
      [ Cmd.map Counter1 counter1Cmd
      , Cmd.map Counter2 counter2Cmd
      ]
    )



-- UPDATE


type Msg
  = Counter1 Counter.Msg
  | UndoCounter1
  | RedoCounter1
  | Counter2 Counter.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Counter1 msg ->
      let
        (model', cmd') = TimeMachine.putOff (Counter.update msg) model.counter1
        counter1' = TimeMachine.modify (\_ -> model') model.counter1
      in
        ( { model
          | counter1 = counter1'
          }
        , Cmd.map Counter1 cmd'
        )

    UndoCounter1 ->
      ( { model
        | counter1 = TimeMachine.undo model.counter1
        }
      , Cmd.none
      )

    RedoCounter1 ->
      ( { model
        | counter1 = TimeMachine.redo model.counter1
        }
      , Cmd.none
      )

    Counter2 msg ->
      let
        (model', cmd') = Counter.update msg model.counter2
      in
        ( { model
          | counter2 = model'
          }
        , Cmd.map Counter2 cmd'
        )



-- VIEW


view : Model -> Html Msg
view model =
  div [class "main"]
    [ div [ class "counter1" ]
      [ TimeMachine.putOff (App.map Counter1 << Counter.view) model.counter1
      , button
        [ type' "button"
        , onClick UndoCounter1
        ]
        [ text "undo" ]
      , button
        [ type' "button"
        , onClick RedoCounter1
        ]
        [ text "redo" ]
      ]
    , App.map Counter2 (Counter.view model.counter2)
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ TimeMachine.putOff (Sub.map Counter1 << Counter.subscriptions) model.counter1
    , Sub.map Counter2 <| Counter.subscriptions model.counter2
    ]
