{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Exercises where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.

{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

type family (x :: Nat) + (y :: Nat) :: Nat where
  'Z + y = y
  ('S x) + y = 'S (x + y)

type family (x :: Nat) ** (y :: Nat) :: Nat where
  'Z ** _ = 'Z
  (S x) ** y = y + (x ** y)


-- Need UndecidableInstances because of the nested type family application

-- | c. Write a function to add two 'SNat' values.

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

addSN :: SNat x -> SNat y -> SNat (x + y)
addSN SZ y = y
addSN (SS x) y = SS $ addSN x y

{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

appendV :: Vector m a -> Vector n a -> Vector (m + n) a
appendV VNil v = v
appendV (VCons a w) v = VCons a $ appendV w v

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

flatMapV :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
flatMapV VNil _ = VNil
flatMapV (VCons a w) f = appendV (f a) (flatMapV w f)

{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

type family (x :: Bool) && (y :: Bool) :: Bool where
  'True && y = y
  'False && _ = 'False

-- | b. Write the type-level @||@ function for booleans.

type family (x :: Bool) || (y :: Bool) :: Bool where
  'True || _ = 'True
  'False || y = y

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.

type family All (xs :: [Bool]) :: Bool where
  All '[x] = x
  All (x ': xs) = x && (All xs)

{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

type family Compare (x :: Nat) (y :: Nat) :: Ordering where
  Compare 'Z 'Z = 'EQ
  Compare 'Z _ = 'LT
  Compare _ 'Z = 'GT
  Compare ('S x) ('S y) = Compare x y

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max 'Z y = y
  Max x 'Z = x
  Max ('S x) ('S y) = 'S (Max x y)

-- | c. Write a family to get the maximum natural in a list.

type family MaxL (xs :: [Nat]) :: Nat where
  MaxL '[x] = x
  MaxL (x ': xs) = Max x (MaxL xs)

{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.

type family If (x :: Bool) y z where
  If 'True y _ = y
  If 'False _ z = z

type family x == y :: Bool where
  x == x = 'True
  _ == _ = 'False

type family InsertTree (x :: Nat) (t :: Tree) :: Tree where
  InsertTree x 'Empty = 'Node 'Empty x 'Empty
  InsertTree x ('Node l n r) =
    If (Compare x n == 'LT)
       ('Node (InsertTree x l) n r)
       ('Node l n (InsertTree x r))

type family InsertNode (x :: Tree) (t :: Tree) :: Tree where
  InsertNode 'Empty t = t
  InsertNode n 'Empty = n
  InsertNode ('Node lx x rx) ('Node ln n rn) =
    If ((Compare x n) == 'LT)
       ('Node (InsertNode ('Node lx x rx) ln) n rn)
       ('Node ln n (InsertNode ('Node lx x rx) rn))


{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.

type family DeleteTree (x :: Nat) (t :: Tree) :: Tree where
  DeleteTree _ 'Empty = 'Empty
  DeleteTree x ('Node l n r) =
    If (x == n)
       (InsertNode l r)
       ('Node (DeleteTree x l) n (DeleteTree x r))

{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': (Append xs ys)

appendH :: HList xs -> HList ys -> HList (Append xs ys)
appendH HNil ys = ys
appendH (HCons x xs) ys = HCons x $ appendH xs ys

{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  Every _ '[] = ()
  Every c (x ': xs) = (c x, Every c xs)
  -- ...

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

instance (Every Show xs) => Show (HList xs) where
  show HNil = "()"
  show (HCons x xs) = show x <> ", " <> show xs

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?

instance (Every Eq xs) => Eq (HList xs) where
  HNil == HNil = True
  (HCons x xs) == (HCons y ys) = x == y && xs == ys

instance (Every Ord xs, Every Eq xs) => Ord (HList xs) where
  compare HNil HNil = EQ
  compare (HCons x xs) (HCons y ys) =
    case compare x y of
      EQ -> compare xs ys
      o -> o

{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

type family AllNats (n :: Nat) :: [Nat] where
  AllNats n = AllNatsHelper n '[]

type family AllNatsHelper (n :: Nat) (ns :: [Nat]) :: [Nat] where
  AllNatsHelper 'Z ns = 'Z ': ns
  AllNatsHelper ('S n) ns = AllNatsHelper n ('S n ': ns)

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?
