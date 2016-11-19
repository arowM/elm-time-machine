module Main exposing (..)

import Html as Html
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import TimeMachine exposing (TimeMachine)
import Counter


-- APP


main : Program Never Model Msg
main =
    Html.program
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


init : ( Model, Cmd Msg )
init =
    let
        ( counter1Model, counter1Cmd ) =
            Counter.init

        ( counter2Model, counter2Cmd ) =
            Counter.init
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Counter1 msg ->
            let
                ( model_, cmd_ ) =
                    TimeMachine.putOff (Counter.update msg) model.counter1

                counter1_ =
                    TimeMachine.modify (\_ -> model_) model.counter1
            in
                ( { model
                    | counter1 = counter1_
                  }
                , Cmd.map Counter1 cmd_
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
                ( model_, cmd_ ) =
                    Counter.update msg model.counter2
            in
                ( { model
                    | counter2 = model_
                  }
                , Cmd.map Counter2 cmd_
                )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ class "main" ]
        [ Html.div [ class "counter1" ]
            [ TimeMachine.putOff (Html.map Counter1 << Counter.view) model.counter1
            , Html.button
                [ type_ "button"
                , onClick UndoCounter1
                ]
                [ Html.text "undo" ]
            , Html.button
                [ type_ "button"
                , onClick RedoCounter1
                ]
                [ Html.text "redo" ]
            ]
        , Html.map Counter2 (Counter.view model.counter2)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ TimeMachine.putOff (Sub.map Counter1 << Counter.subscriptions) model.counter1
        , Sub.map Counter2 <| Counter.subscriptions model.counter2
        ]
