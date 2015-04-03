module IntDict.Safe
    ( InvalidKey (..)
    , SafeKeyResult
    , safeInsert, safeRemove, safeUpdate
    , safeMember, safeGet
    ) where

import IntDict (..)


type InvalidKey = InvalidKey


type alias SafeKeyResult a = Result InvalidKey a


safeWrapper : Int -> (() -> a) -> SafeKeyResult a
safeWrapper k f =
    if not (isValidKey k)
    then Err InvalidKey
    else Ok (f ())


safeInsert : Int -> v -> IntDict v -> SafeKeyResult (IntDict v)
safeInsert k v dict =
    safeWrapper k <| \() -> insert k v dict


safeRemove : Int -> IntDict v -> SafeKeyResult (IntDict v)
safeRemove k dict =
    safeWrapper k <| \() -> remove k dict


safeUpdate : Int -> (Maybe v -> Maybe v) -> IntDict v -> SafeKeyResult (IntDict v)
safeUpdate k alter dict =
    safeWrapper k <| \() -> update k alter dict


safeGet : Int -> IntDict v -> SafeKeyResult (Maybe v)
safeGet k dict =
    safeWrapper k <| \() -> get k dict


safeMember : Int -> IntDict v -> SafeKeyResult Bool
safeMember k dict =
    safeWrapper k <| \() -> member k dict