module Data.Status
  exposing
    ( Data(..)
    , Status(..)
    , data
    , failure
    , fromResult
    , isLoading
    , isWaiting
    , loading
    , map
    , mapWithCmd
    , refresh
    , reload
    , success
    , toMaybe
    , waiting
    )

{-|

@docs Status, Data, data, waiting, loading, success, failure, reload, refresh
@docs isLoading, isWaiting
@docs fromResult, toMaybe
@docs map, mapWithCmd

-}


{-| -}
type Status error data
  = Loading (Data error data)
  | Finished (Data error data)


{-| -}
type Data error data
  = Nothing
  | Success data
  | Failure error


{-| -}
waiting : Status error data
waiting =
  Finished Nothing


{-| -}
loading : Status error data
loading =
  Loading Nothing


{-| -}
reload : Status error data -> Status error data
reload status =
  Loading (data status)


{-| -}
success : data -> Status error data
success new =
  Finished (Success new)


{-| -}
refresh : (data -> data -> data) -> data -> Status error data -> Status error data
refresh add new status =
  Finished <|
    case data status of
      Success existing ->
        Success (add existing new)

      _ ->
        Success new


{-| -}
failure : error -> Status error data
failure err =
  Finished (Failure err)



-- GETTERS


{-| -}
isWaiting : Status error data -> Bool
isWaiting status =
  case status of
    Finished Nothing ->
      True

    _ ->
      False


{-| -}
isLoading : Status error data -> Bool
isLoading status =
  case status of
    Loading _ ->
      True

    _ ->
      False


{-| -}
data : Status error data -> Data error data
data status =
  case status of
    Loading data ->
      data

    Finished data ->
      data



-- HELPERS


{-| -}
map : (data -> data) -> Status error data -> Status error data
map update status =
  case status of
    Loading data ->
      Loading <|
        case data of
          Success existing ->
            Success (update existing)

          _ ->
            data

    Finished data ->
      Finished <|
        case data of
          Success existing ->
            Success (update existing)

          _ ->
            data


{-| -}
mapWithCmd : (data -> ( data, Cmd msg )) -> Status error data -> ( Status error data, Cmd msg )
mapWithCmd update status =
  case status of
    Loading data ->
      case data of
        Success existing ->
          Tuple.mapFirst (Loading << Success) (update existing)

        _ ->
          ( Loading data, Cmd.none )

    Finished data ->
        case data of
          Success existing ->
            Tuple.mapFirst (Finished << Success) (update existing)

          _ ->
            ( Finished data, Cmd.none )


{-| -}
toMaybe : Status error data -> Maybe data
toMaybe status =
  case data status of
    Success data ->
      Maybe.Just data

    _ ->
      Maybe.Nothing


{-| -}
fromResult : Result error data -> Status error data
fromResult result =
  case result of
    Ok data ->
      Finished (Success data)

    Err err ->
      Finished (Failure err)
