{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Exercises where





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  Nil :: CountableList
  Cons :: (Countable a) => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList (Cons a cl) = count a + countList cl
countList Nil = 0


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero (Cons a cl) = if count a == 0
                         then dropZero cl
                         else Cons a (dropZero cl)


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Cannot implement; we have no type information about `a` except that it is `Countable`"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  ANil :: AnyList
  ACons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList al = go al ANil
  where go ANil rev = rev
        go (ACons a al) rev = go al (ACons a rev)

filterAnyList :: a -> Bool -> AnyList -> AnyList
filterAnyList = error "don't know `a`, or anything else, so we can't write a predicate that typechecks"

lengthAnyList :: AnyList -> Int
lengthAnyList ANil = 0
lengthAnyList (ACons _ al) = 1 + lengthAnyList al

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = error "they may all be monoids, but there's no guarantee they're the _same_ monoid. No parametricity here!"

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList ANil = True
isEmptyAnyList (ACons _ _) = False

instance Show AnyList where
  show ANil = "I'm an empty list of anything"
  show (ACons _ _) = "I'm a list of anything, but I don't know how to show the things because there's no show constraint"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- `input` is existential; all we can do is call the provided function with the
-- provided input

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance (Eq o) => Eq (TransformableTo o) where
  (TransformWith f x) == (TransformWith g y) = f x == g y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap f (TransformWith g x) = TransformWith (f . g) x

{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

areEqual :: EqPair -> Bool
areEqual (EqPair a b) = a == b

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

data EqPair'' a = (Eq a) => EqPair'' a a

{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox i _)) = i

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _ mb) = 1 + countLayers mb
countLayers (StringBox _ mb) = 1 + countLayers mb
countLayers (BoolBox _ mb) = 1 + countLayers mb

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- removeLayer :: MysteryBox a -> MysteryBox b
-- removeLayer EmptyBox = EmptyBox
-- removeLayer (IntBox _ mb) = mb
-- removeLayer (StringBox _ mb) = mb
-- removeLayer (BoolBox _ mb) = mb
-- `b` isn't actually "free". The return type needs to depend on the input type


{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hhead :: HList (a, b) -> a
hhead (HCons h _) = h

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

-- No, the compiler can't figure out what `head` and `tail` are in `(head, tail)` becaues the tuples aren't nested

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- no, we need to unpack and repack the tuples into the types
-- So some type level function like is necessary
-- concatType (h, ()) l = (h, l)
-- concatType (h, t) l = (h, concatType t l)


{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HTEmpty :: HTree Empty
  HTBranch :: HTree l -> c -> HTree r -> HTree (Branch l c r)
  -- ...

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch l c r) -> HTree (Branch Empty c r)
deleteLeft (HTBranch l c r) = HTBranch HTEmpty c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  ht1 == ht2 = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  (HTBranch l c r) == (HTBranch l' c' r') = l == l' && c == c' && r == r'


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  AltCons :: a -> AlternatingList b a -> AlternatingList a b
  AltNil :: AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts (AltCons a (AltCons _ abs)) = a : getFirsts abs
getFirsts (AltCons a AltNil) = a : []
getFirsts AltNil = []

getSeconds :: AlternatingList a b -> [b]
getSeconds (AltCons _ (AltCons b abs)) = b : getSeconds abs
getSeconds (AltCons _ AltNil) = []
getSeconds AltNil = []

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues abs = go abs (mempty, mempty)
  where go AltNil r = r
        go (AltCons a (AltCons b abs)) (as, bs) = go abs (a <> as, b <> bs)
        go (AltCons a AltNil) (as, bs) = (a <> as, bs)





{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals x y) = eval x == eval y
eval (Add x y) = eval x + eval y
eval (If tst t f) = if (eval tst)
                      then eval t
                      else eval f
eval (IntValue x) = x
eval (BoolValue b) = b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse = parseInt

parseInt :: DirtyExpr -> Maybe (Expr Int)
parseInt (DirtyEquals _ _) = Nothing
parseInt (DirtyAdd e1 e2) = Add <$> parseInt e1 <*> parseInt e2
parseInt (DirtyIf tst t f) = If <$> parseBool tst <*> parseInt t <*> parseInt f
parseInt (DirtyIntValue x) = Just $ IntValue x
parseInt (DirtyBoolValue _) = Nothing

parseBool :: DirtyExpr -> Maybe (Expr Bool)
parseBool (DirtyEquals e1 e2) = Equals <$> parseInt e1 <*> parseInt e2
parseBool (DirtyAdd _ _) = Nothing
parseBool (DirtyIf tst t f) = If <$> parseBool tst <*> parseBool t <*> parseBool f
parseBool (DirtyIntValue _) = Nothing
parseBool (DirtyBoolValue b) = Just $ BoolValue b

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?

-- to add functions, we could add the following cases to the Expr gadt

data LamExpr a where
  Lam :: (a -> b) -> LamExpr (a -> b)
  App :: LamExpr (a -> b) -> a -> LamExpr b

-- and avoid Maybe in the evaluator by augmenting eval with

evalLam :: LamExpr a -> a
evalLam (Lam f) = f
evalLam (App f a) = evalLam f a


{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  Id :: TypeAlignedList a a
  Fn :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

-- | b. Which types are existential?

-- all of the types in the "middle" of the composition chain

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs Id Id = Id
composeTALs Id tal = tal
composeTALs tal Id = tal
composeTALs bc (Fn aa' a'b) = Fn aa' $ composeTALs bc a'b

pipe :: TypeAlignedList a b -> a -> b
pipe Id a = a
pipe (Fn f tal) a = pipe tal (f a)
