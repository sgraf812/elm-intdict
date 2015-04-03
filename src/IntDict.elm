module IntDict 
    ( IntDict
    , isValidKey
    , empty, singleton, insert, update, remove
    , member, get
    , filter, map, foldl, foldr, partition
    , union, intersect, diff
    , keys, values, toList, fromList
    , toString'
    ) where

import Bitwise
import Maybe (Maybe (..))
import List


type alias KeyPrefix =
    { prefixBits : Int
    , branchingBit : Int -- already in 2^i form -> always > 0
    }


type IntDict a
    = Empty
    | Leaf { key : Int, value : a }
    | Inner { prefix : KeyPrefix, left : IntDict a, right : IntDict a }


isValidKey : Int -> Bool
isValidKey n =              -- perform some dirty JS magic to turn the double
    n `Bitwise.or` 0 == n   -- into an integer. We can then check for overflow.
                            -- This seems easier than checking for 32 bits.
                            -- `or` 0 is similar to `mod` <32bits>


-- SMART CONSTRUCTORS

empty : IntDict a
empty = Empty


singleton : Int -> a -> IntDict a
singleton k v =
    leaf k v


inner : KeyPrefix -> IntDict a -> IntDict a -> IntDict a
inner p l r =
    case (l, r) of
        (Empty, _) -> r
        (_, Empty) -> l
        (_, _) -> Inner
            { prefix = p
            , left = l
            , right = r
            }


leaf : Int -> a -> IntDict a
leaf k v = Leaf
    { key = k
    , value = v
    }


{-| Consider a branchingBit of 2^4 = 16 = 0b00010000.
Then branchingBit*2-1 = 2^5-1  = 31 = 0b00011111,
Now apply bitwise NOT to get the mask 0b11100000.
-}
bitMask : KeyPrefix -> Int
bitMask p =
    Bitwise.complement <| p.branchingBit*2 - 1


prefixMatches : KeyPrefix -> Int -> Bool
prefixMatches p n =
    n `Bitwise.and` bitMask p == p.prefixBits


isBranchingBitSet : KeyPrefix -> Int -> Bool
isBranchingBitSet p n =
    n `Bitwise.and` p.branchingBit /= 0


member : Int -> IntDict a -> Bool
member k dict = 
    case get k dict of
        Just _ -> True
        Nothing -> False


get : Int -> IntDict a -> Maybe a
get k dict =
    case dict of
        Empty ->
            Nothing
        Leaf l ->
            if l.key == k
            then Just l.value
            else Nothing
        Inner i ->
            if not (prefixMatches i.prefix k)
            then Nothing
            else if isBranchingBitSet i.prefix k    -- continue in left or right branch, 
                 then get k i.right              -- depending on whether the branching 
                 else get k i.left               -- bit is set in the key


type SafeLookupResult a
    = FoundValue a
    | NotFound
    | InvalidKey


safeGet : Int -> IntDict a -> SafeLookupResult a
safeGet k dict =
    if not (isValidKey k)
    then InvalidKey
    else case get k dict of
        Just v -> FoundValue v
        Nothing -> NotFound


{-| Compute the longest common prefix of two keys.
Returns 0 as branchingBit if equal.

Find the highest bit not set in

    diff = x `xor` y -- 0b011001 `xor` 0b011010 = 0b000011
    
via http://aggregate.org/MAGIC/#Most Significant 1 Bit
-}
lcp : Int -> Int -> KeyPrefix
lcp x y =
    let diff = x `Bitwise.xor` y
        (&) = Bitwise.and
        (>>>) = Bitwise.shiftRightLogical
        (.|.) = Bitwise.or
        shiftOr d shift = d .|. (d >>> shift)
        d1 = shiftOr diff 1 
        d2 = shiftOr d1 2
        d3 = shiftOr d2 4
        d4 = shiftOr d3 8
        d5 = shiftOr d4 16
        -- d6 = shiftOr d5 32 -- 64 bit support?!
        -- d5 has the same msb set as diff. However, all
        -- bits below the msb are also 1! This means we can
        -- do the following to get the msb:
        branchingBit = d5 & Bitwise.complement (d5 >>> 1)
        mask = bitMask { prefixBits = 0, branchingBit = branchingBit } -- feels hacky
        prefixBits = x & mask -- should equal y & mask
    in 
        { prefixBits = prefixBits
        , branchingBit = branchingBit 
        }

{- precondition: k1 /= k2 -}
join : (Int, IntDict a) -> (Int, IntDict a) -> IntDict a
join (k1, d1) (k2, d2) =
    let prefix = lcp k1 k2
    in if isBranchingBitSet prefix k2 -- if so, d2 will be the right child
       then inner prefix d1 d2
       else inner prefix d2 d1


insert : Int -> a -> IntDict a -> IntDict a
insert k v dict =
    update k (always (Just v)) dict


remove : Int -> IntDict a -> IntDict a
remove k dict =
    update k (always Nothing) dict


update : Int -> (Maybe a -> Maybe a) -> IntDict a -> IntDict a
update k alter dict =
    let alteredNode v = 
            case alter v of -- handle this centrally
                Just v' -> leaf k v'
                Nothing -> empty -- The inner constructor will do the rest
    in case dict of
        Empty -> 
           alteredNode Nothing
        Leaf l ->
            if l.key == k 
            then alteredNode (Just l.value)                     -- This updates or removes the leaf with the same key
            else join (k, alteredNode Nothing) (l.key, dict)    -- This potentially inserts a new node
        Inner i ->
            if prefixMatches i.prefix k
            then if isBranchingBitSet i.prefix k
                 then inner i.prefix i.left (update k alter i.right)
                 else inner i.prefix (update k alter i.left) i.right
            else -- we have to join a new leaf with the current diverging Inner node
                join (k, alteredNode Nothing) (i.prefix.prefixBits, dict)


{-| Keep a key-value pair when it satisfies a predicate. -}
filter : (Int -> v -> Bool) -> IntDict v -> IntDict v
filter predicate dict =
    let add k v d =
            if predicate k v
            then insert k v d
            else d
    in foldl add empty dict


map : (Int -> a -> b) -> IntDict a -> IntDict b
map f dict =
    case dict of
        Empty -> empty
        Leaf l -> leaf l.key (f l.key l.value)
        Inner i -> Inner { i | left <- map f i.left, right <- map f i.right }


foldl : (Int -> v -> a -> a) -> a -> IntDict v -> a
foldl f acc dict =
    case dict of
        Empty -> acc
        Leaf l ->
            f l.key l.value acc
        Inner i ->
            foldl f (foldl f acc i.left) i.right


foldr : (Int -> v -> a -> a) -> a -> IntDict v -> a
foldr f acc dict =
    case dict of
        Empty -> acc
        Leaf l ->
            f l.key l.value acc
        Inner i ->
            foldr f (foldr f acc i.right) i.left


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (Int -> v -> Bool) -> IntDict v -> (IntDict v, IntDict v)
partition predicate dict =
    let add key value (t1, t2) =
            if predicate key value
                then (insert key value t1, t2)
                else (t1, insert key value t2)
    in
        foldl add (empty, empty) dict


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary. -}
union : IntDict v -> IntDict v -> IntDict v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary. -}
intersect : IntDict v -> IntDict v -> IntDict v
intersect t1 t2 =
    filter (\k _ -> k `member` t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
Preference is given to the first dictionary. -}
diff : IntDict v -> IntDict v -> IntDict v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| Get all of the keys in a dictionary. -}
keys : IntDict v -> List Int
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary. -}
values : IntDict v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


fromList : List (Int, a) -> IntDict a
fromList pairs =
    let insert' (k, v) dict = insert k v dict
    in List.foldl insert' empty pairs


toList : IntDict a -> List (Int, a)
toList dict = 
    foldr (\key value list -> (key, value) :: list) [] dict


toString' : IntDict a -> String
toString' dict = "IntDict.fromList " ++ toString (toList dict)
