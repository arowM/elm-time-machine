module TimeMachine exposing
  ( TimeMachine
  , succeed
  , modify
  , putOff
  , undo
  , mayUndo
  , redo
  , mayRedo
  )

{-| This module empower any model to undo/redo in elm architecture way.

# Types

@docs TimeMachine

# Operators for a crew of the time machine

@docs succeed
@docs modify
@docs putOff

# Operators to steer the time machine

@docs undo
@docs mayUndo
@docs redo
@docs mayRedo

-}



-- Types


type TimeMachine a = TimeMachine
    { current : a
    , past : List a
    , future : List a
    }



-- Operators for a crew of the time machine



{-| The `succeed` just put a model into the time machine.
This is the first step if you want to enable the model to undo/redo.
-}
succeed : a -> TimeMachine a
succeed a = TimeMachine
  { current = a
  , past = []
  , future = []
  }


{-| The way to edit crew (i.e., model in the `TimeMachine`) from outside of the time machine.
-}
modify : (a -> a) -> TimeMachine a -> TimeMachine a
modify f (TimeMachine o) = TimeMachine
  { current = f o.current
  , past = o.current :: o.past
  , future = []
  }


{-| The way to put the crew off.
-}
putOff : (a -> b) -> TimeMachine a -> b
putOff f (TimeMachine o) = f o.current



-- Operators to steer the time machine


{-| A function to get past state of the model.
If no more past state exists, just returns as it is.
-}
undo : TimeMachine a -> TimeMachine a
undo (TimeMachine o) =
  case o.past of
    [] ->
      TimeMachine o
    (p :: ps) ->
      TimeMachine
        { current = p
        , past = ps
        , future = o.current :: o.future
        }


{-| Same as `undo` but returns `Nothing` if there is no past state.
-}
mayUndo : TimeMachine a -> Maybe (TimeMachine a)
mayUndo (TimeMachine o) =
  case o.past of
    [] ->
      Nothing
    (p :: ps) ->
      Just <| TimeMachine
        { current = p
        , past = ps
        , future = o.current :: o.future
        }


{-| A function to get future state of the model.
If no more future state exists, just returns as it is.
-}
redo : TimeMachine a -> TimeMachine a
redo (TimeMachine o) =
  case o.future of
    [] ->
      TimeMachine o
    (f :: fs) ->
      TimeMachine
        { current = f
        , past = o.current :: o.past
        , future = fs
        }


{-| Same as `undo` but returns `Nothing` if there is no future state.
-}
mayRedo : TimeMachine a -> Maybe (TimeMachine a)
mayRedo (TimeMachine o) =
  case o.future of
    [] ->
      Nothing
    (f :: fs) ->
      Just <| TimeMachine
        { current = f
        , past = o.current :: o.past
        , future = fs
        }
