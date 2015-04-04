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


{-| `IntDict` is an optimized version of a `Dict` with `Int` as key type. 

Its API is modeled `Dict`, so it can be regarded as a drop-in replacement.
If you are comfortable with `Dict`, working with `IntDict` should be a breeze!

#Technicalities

Since JavaScript's number type is kind of messed up, Elm's `Int` is not particularly
well-behaved wrt. bitwise operations. Currently, JS supports 32 bit integers, so there
probably enough room for key picks. **However, when sanitizing user input, it is mandatory
that a prior `isValidKey` or one of the safe versions in `IntDict.Safe` is used!** This is
to prevent the overflow behavior.

This library is inspired by Haskells [IntMap](http://hackage.haskell.org/package/containers-0.2.0.1/docs/Data-IntMap.html), 
which in turn implements Okasaki and Gill's [Fast mergable integer maps](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf).

As noted in the references, here are some runtimes (W is the number of bits in `Int`, a constant!):

*O(min(n, W))*: `insert`, `update`, `remove`, `get`, `member`

# Build
@docs empty, singleton, insert, update, remove
# Query
@docs member, get
# Combine
@docs union, intersect, diff
# Lists
@docs keys, values, toList, fromList
# Transform
@docs map, foldl, foldr, filter, partition

-}


import Bitwise
import Maybe (Maybe (..))
import List


type alias KeyPrefix =
    { prefixBits : Int
    , branchingBit : Int -- already in 2^i form -> always > 0
    }


type IntDict v
    = Empty
    | Leaf { key : Int, value : v }
    | Inner { prefix : KeyPrefix, left : IntDict v, right : IntDict v }


{-| Validates that a given integer is usable as a key.
This is necessary due to JavaScript's weird number type.
Basically this assures that we can use the functions
from `Bitwise` without risking integer overflow.

**This function is a necessity for sanitizing user input!** Alternatively,
use the safe functions from `IntDict.Safe` which perform the check for you.

As with the current version of JavaScript (2015), only 32 bit signed integers are supported.
If this ever changes, contact me! Certain parts of the implementation depend on this! -}
isValidKey : Int -> Bool
isValidKey k =              -- perform some dirty JS magic to turn the double
    k `Bitwise.or` 0 == k   -- into an integer. We can then check for overflow.
                            -- This seems easier than checking for 32 bits.
                            -- `or` 0 is similar to `mod` <32bits>


-- SMART CONSTRUCTORS

-- not exported
inner : KeyPrefix -> IntDict v -> IntDict v -> IntDict v
inner p l r =
    case (l, r) of
        (Empty, _) -> r
        (_, Empty) -> l
        (_, _) -> Inner
            { prefix = p
            , left = l
            , right = r
            }

-- exported as the singleton alias
leaf : Int -> v -> IntDict v
leaf k v = Leaf
    { key = k
    , value = v
    }

-- SOME PRIMITIVES

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


{- Clear all bits other than the highest in n.
Assumes n to be positive! For implementation notes, see [this](http://aggregate.org/MAGIC/#Most Significant 1 Bit).
-}
highestBitSet : Int -> Int
highestBitSet n =
    let shiftOr n' shift = n' `Bitwise.or` (n' `Bitwise.shiftRightLogical` shift)
        n1 = shiftOr n 1 
        n2 = shiftOr n1 2
        n3 = shiftOr n2 4
        n4 = shiftOr n3 8
        n5 = shiftOr n4 16
        -- n6 = shiftOr n5 32 -- 64 bit support?!
        -- n5 has the same msb set as diff. However, all
        -- bits below the msb are also 1! This means we can
        -- do the following to get the msb:
    in n5 `Bitwise.and` Bitwise.complement (n5 `Bitwise.shiftRightLogical` 1)


{- Compute the longest common prefix of two keys.
Returns 0 as branchingBit if equal.

Find the highest bit not set in

    diff = x `xor` y -- 0b011001 `xor` 0b011010 = 0b000011 

-}
lcp : Int -> Int -> KeyPrefix
lcp x y =
    let diff = x `Bitwise.xor` y
        prefix = { prefixBits = 0, branchingBit = highestBitSet diff }
        mask = bitMask prefix   -- feels hacky, because prefixBits isn't yet set
        prefixBits = x `Bitwise.and` mask   -- should equal y & mask
    in { prefix | prefixBits <- prefixBits }  -- branchingBit is already set


signBit : Int
signBit =
    highestBitSet -1 

isBranchingBitSet : KeyPrefix -> Int -> Bool
isBranchingBitSet p n =
    let n' = n `Bitwise.xor` signBit -- This is a hack that fixes the ordering of keys.
    in n' `Bitwise.and` p.branchingBit /= 0


-- BUILD


{-| Create an empty dictionary. -}
empty : IntDict v
empty = Empty


{-| Create a dictionary with one key-value pair. -}
singleton : Int -> v -> IntDict v
singleton key value =
    leaf key value


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : Int -> v -> IntDict v -> IntDict v
insert key value dict =
    update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : Int -> IntDict v -> IntDict v
remove key dict =
    update key (always Nothing) dict


{-| Update the value of a dictionary for a specific key with a given function. -}
update : Int -> (Maybe v -> Maybe v) -> IntDict v -> IntDict v
update key alter dict =
    let alteredNode v = 
            case alter v of                                     -- handle this centrally
                Just v' -> leaf key v'
                Nothing -> empty                                -- The inner constructor will do the rest

        join (k1, d1) (k2, d2) =                                -- precondition: k1 /= k2
            let prefix = lcp k1 k2
            in if isBranchingBitSet prefix k2                   -- if so, d2 will be the right child
               then inner prefix d1 d2
               else inner prefix d2 d1

    in case dict of
        Empty -> 
           alteredNode Nothing
        Leaf l ->
            if l.key == key 
            then alteredNode (Just l.value)                     -- This updates or removes the leaf with the same key
            else join (key, alteredNode Nothing) (l.key, dict)    -- This potentially inserts a new node
        Inner i ->
            if prefixMatches i.prefix key
            then if isBranchingBitSet i.prefix key
                 then inner i.prefix i.left (update key alter i.right)
                 else inner i.prefix (update key alter i.left) i.right
            else -- we have to join a new leaf with the current diverging Inner node
                join (key, alteredNode Nothing) (i.prefix.prefixBits, dict)


-- QUERY


{-| Determine if a key is in a dictionary. -}
member : Int -> IntDict v -> Bool
member key dict = 
    case get key dict of
        Just _ -> True
        Nothing -> False


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary. -}
get : Int -> IntDict v -> Maybe v
get key dict =
    case dict of
        Empty ->
            Nothing
        Leaf l ->
            if l.key == key
            then Just l.value
            else Nothing
        Inner i ->
            if not (prefixMatches i.prefix key)
            then Nothing
            else if isBranchingBitSet i.prefix key -- continue in left or right branch, 
                 then get key i.right              -- depending on whether the branching 
                 else get key i.left               -- bit is set in the key


-- TRANSFORM


{-| Keep a key-value pair when it satisfies a predicate. -}
filter : (Int -> v -> Bool) -> IntDict v -> IntDict v
filter predicate dict =
    let add k v d =
            if predicate k v
            then insert k v d
            else d
    in foldl add empty dict


{-| Apply a function to all values in a dictionary. -}
map : (Int -> a -> b) -> IntDict a -> IntDict b
map f dict =
    case dict of
        Empty -> empty
        Leaf l -> leaf l.key (f l.key l.value)
        Inner i -> Inner { i | left <- map f i.left, right <- map f i.right }

{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key. -}
foldl : (Int -> v -> a -> a) -> a -> IntDict v -> a
foldl f acc dict =
    case dict of
        Empty -> acc
        Leaf l ->
            f l.key l.value acc
        Inner i ->
            foldl f (foldl f acc i.left) i.right

{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key. -}
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
contains the rest. -}
partition : (Int -> v -> Bool) -> IntDict v -> (IntDict v, IntDict v)
partition predicate dict =
    let add key value (t1, t2) =
            if predicate key value
                then (insert key value t1, t2)
                else (t1, insert key value t2)
    in
        foldl add (empty, empty) dict


-- COMBINE


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


-- LISTS


{-| Get all of the keys in a dictionary. -}
keys : IntDict v -> List Int
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary. -}
values : IntDict v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs. -}
toList : IntDict v -> List (Int, v)
toList dict = 
    foldr (\key value list -> (key, value) :: list) [] dict


{-| Convert an association list into a dictionary. -}
fromList : List (Int, v) -> IntDict v
fromList pairs =
    let insert' (k, v) dict = insert k v dict
    in List.foldl insert' empty pairs


-- STRING REPRESENTATION


{-| Generates a string representation similar to what `toString` 
generates for `Dict`. -}
toString' : IntDict v -> String
toString' dict = "IntDict.fromList " ++ toString (toList dict)
