{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Exercises where

import Prelude hiding (length, (!!))

import Data.Kind (Type, Constraint)
import Data.Function ((&))
import System.IO (openFile, IOMode(ReadWriteMode), hPutStr, hClose, Handle, hGetContents)





{- ONE -}

-- | One of the restrictions around classes that we occasionally hit is that we
-- can only have one instance for a type. There are, for example, two good
-- candidates for a monoid instance when we think about 'Integer':

data IntegerMonoid = Sum | Product

-- | a. Write a newtype around 'Integer' that lets us choose which instance we
-- want.

newtype IntMonoid (m :: IntegerMonoid) = IntMonoid { getInt :: Integer }

-- | b. Write the two monoid instances for 'Integer'.

instance Semigroup (IntMonoid 'Sum) where
  (IntMonoid x) <> (IntMonoid y) = IntMonoid $ x + y

instance Monoid (IntMonoid 'Sum) where
  mempty = IntMonoid 0

instance Semigroup (IntMonoid 'Product) where
  (IntMonoid x) <> (IntMonoid y) = IntMonoid $ x * y

instance Monoid (IntMonoid 'Product) where
  mempty = IntMonoid 1

-- | c. Why do we need @FlexibleInstances@ to do this?

-- Because (IntMonoid 'Sum) has a concrete 'a', not a variable





{- TWO -}

-- | We can write a type that /is/ of kind 'Type', but has no value-level
-- members. We usually call this type 'Void':

data Void -- No constructors!

-- | a. If we promote this with DataKinds, can we produce any /types/ of kind
-- 'Void'?

class IsVoid (a :: Void)

-- no instances, because there are no data constructors for the type Void
-- promoted to the type level to become types of kind Void

-- | b. What are the possible type-level values of kind 'Maybe Void'?

class MaybeVoid (a :: Maybe Void)
instance MaybeVoid 'Nothing

-- | c. Considering 'Maybe Void', and similar examples of kinds such as
-- 'Either Void Bool', why do you think 'Void' might be a useful kind?

-- can signal that one type constructor of a kind will not be used
-- ie Either Void Type guarantees a Right Type

{- THREE -}

-- | a. Write a GADT that holds strings or integers, and keeps track of how
-- many strings are present. Note that you might need more than 'Nil' and
-- 'Cons' this time...

data Nat = Z | S Nat

data StringAndIntList (stringCount :: Nat) (intCount :: Nat) where
  None :: StringAndIntList 'Z 'Z
  CS :: String -> StringAndIntList n m -> StringAndIntList (S n) m
  CN :: Int -> StringAndIntList n m -> StringAndIntList n (S m)

-- | b. Update it to keep track of the count of strings /and/ integers.

-- | c. What would be the type of the 'head' function?

stringHead :: StringAndIntList (S n) m -> String
stringHead (CS s _) = s
stringHead (CN _ l) = stringHead l

intHead :: StringAndIntList n (S m) -> Int
intHead (CN i _) = i
intHead (CS _ l) = intHead l

siHead :: StringAndIntList (S n) (S m) -> Either String Int
siHead (CS s _) = Left s
siHead (CN i _) = Right i

-- soon, I imagine, we'll learn how to say (n + m > 0) => StringAndIntList n m
-- and then we can use that type in siHead for more flexibility

{- FOUR -}

-- | When we talked about GADTs, we discussed existentials, and how we could
-- only know something about our value if the context told us:

data Showable where
  Showable :: Show a => a -> Showable

-- | a. Write a GADT that holds something that may or may not be showable, and
-- stores this fact in the type-level.

data MaybeShowable (isShowable :: Bool) where
  CanShow :: (Show a) => a -> MaybeShowable 'True
  Can'tShow :: a -> MaybeShowable 'False

-- there's nothing to stop us from calling @Can'tShow Int@, which is in fact an
-- instance of @Show@

-- | b. Write a 'Show' instance for 'MaybeShowable'. Your instance should not
-- work unless the type is actually 'show'able.

instance Show (MaybeShowable 'True) where
  show (CanShow a) = show a

-- | c. What if we wanted to generalise this to @Constrainable@, such that it
-- would work for any user-supplied constraint of kind 'Constraint'? How would
-- the type change? What would the constructor look like? Try to build this
-- type - GHC should tell you exactly which extension you're missing.

data MaybeConstrained (c :: Maybe (Type -> Constraint)) where
  IsConstrained :: (c a) => a -> MaybeConstrained ('Just c)
  IsNotConstrained :: a -> MaybeConstrained 'Nothing



{- FIVE -}

-- | Recall our list type:

data List a = Nil | Cons a (List a)

-- | a. Use this to write a better 'HList' type than we had in the @GADTs@
-- exercise. Bear in mind that, at the type-level, 'Nil' and 'Cons' should be
-- "ticked". Remember also that, at the type-level, there's nothing weird about
-- having a list of types!

data HList (types :: List Type) where
  HNil  :: HList 'Nil
  HCons :: a -> HList ts -> HList ('Cons a ts)

-- | b. Write a well-typed, 'Maybe'-less implementation for the 'tail' function
-- on 'HList'.

hTail :: HList ('Cons t ts) -> HList ts
hTail (HCons _ ts) = ts

-- | c. Could we write the 'take' function? What would its type be? What would
-- get in our way?

hTake :: Int -> HList ts -> HList us
hTake = undefined
-- hTake 0 _ = HNil
-- hTake n HNil = HNil
-- hTake n (HCons t us) = HCons t $ hTake (n - 1) us

-- ghc can't unify us with ts for every recursive call, because they change in
-- every recursive call. We'd need something like
-- hTake :: (Int ~ n) => n -> HList ts -> HList (take n ts)


{- SIX -}

-- | Here's a boring data type:

data For = Everyone | Admin

data BlogAction (for :: For) where
  AddBlog :: BlogAction 'Everyone
  DeleteBlog :: BlogAction 'Admin
  AddComment :: BlogAction 'Everyone
  DeleteComment :: BlogAction 'Admin

-- | a. Two of these actions, 'DeleteBlog' and 'DeleteComment', should be
-- admin-only. Extend the 'BlogAction' type (perhaps with a GADT...) to
-- express, at the type-level, whether the value is an admin-only operation.
-- Remember that, by switching on @DataKinds@, we have access to a promoted
-- version of 'Bool'!

-- | b. Write a 'BlogAction' list type that requires all its members to be
-- the same "access level": "admin" or "non-admin".

data BlogActionList (isSafe :: For) where
  BlogNil :: BlogActionList isSafe
  BlogCons :: BlogAction isSafe -> BlogActionList isSafe -> BlogActionList isSafe

-- | c. Let's imagine that our requirements change, and 'DeleteComment' is now
-- available to a third role: moderators. Could we use 'DataKinds' to introduce
-- the three roles at the type-level, and modify our type to keep track of
-- this?

data ForMore = Everyone' | Moderators | Admin' -- and use this instead of For above



{- SEVEN -}

-- | When we start thinking about type-level Haskell, we inevitably end up
-- thinking about /singletons/. Singleton types have a one-to-one value-type
-- correspondence - only one value for each type, only one type for each value.
-- A simple example is '()', whose only value is '()'. 'Bool' is /not/ a
-- singleton, because it has multiple values.

-- We can, however, /build/ a singleton type for 'Bool':

data SBool (value :: Bool) where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

-- | a. Write a singleton type for natural numbers:

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)
  -- ...

-- | b. Write a function that extracts a vector's length at the type level:

length :: Vector n a -> SNat n
length VNil = SZ
length (VCons _ v) = SS $ length v

-- | c. Is 'Proxy' a singleton type?

data Proxy a = Proxy

-- no: there is only one value for each type (ie 'Proxy'), but for the sole
-- value (ie 'Proxy'), there are many types (ie any possible 'a')





{- EIGHT -}

-- | Let's imagine we're writing some Industry Haskell™, and we need to read
-- and write to a file. To do this, we might write a data type to express our
-- intentions:

data FileStatus = FOpen | FClosed

data Program (fs :: FileStatus) r where
  OpenFile :: Program 'FOpen Handle
  WriteFile :: String  -> Program 'FOpen ()
  ReadFile  ::(String -> Program 'FOpen r) -> Program 'FOpen Handle -> Program 'FOpen r
  CloseFile :: Program 'FOpen r -> Program 'FClosed ()
  Exit :: Program 'FClosed r -> Program 'FClosed r
  Noop :: Program fs ()

-- | We could then write a program like this to use our language:

myApp :: Program 'FClosed ()
myApp =
  Exit $ CloseFile $
    ReadFile (\contents ->
      if contents == "WHAT"
        then WriteFile "...bug?"
        else Noop
      )
      OpenFile

-- | ... but wait, there's a bug! If the contents of the file equal "WHAT", we
-- forget to close the file! Ideally, we would like the compiler to help us: we
-- could keep track of whether the file is open at the type level!
--
-- - We should /not/ be allowed to open a file if another file is currently
-- open.
--
-- - We should /not/ be allowed to close a file unless a file is open.
--
-- If we had this at the type level, the compiler should have been able to tell
-- us that the branches of the @if@ have different types, and this program
-- should never have made it into production. We should also have to say in the
-- type of 'myApp' that, once the program has completed, the file will be
-- closed.

-- | Improve the 'Program' type to keep track of whether a file is open.  Make
-- sure the constructors respect this flag: we shouldn't be able to read or
-- write to the file unless it's open. This exercise is a bit brain-bending;
-- why? How could we make it more intuitive to write?

-- ironically -- Monads

-- | EXTRA: write an interpreter for this program. Nothing to do with data
-- kinds, but a nice little problem.

interpret :: String -> Program fs a -> IO a
interpret fileName = \case
  OpenFile -> openFile fileName ReadWriteMode
  WriteFile contents -> hPutStr undefined contents
  ReadFile f p -> do
    h <- interpret fileName p
    contents <- hGetContents h
    interpret fileName $ f contents
  CloseFile p -> hClose undefined
  Exit p -> interpret fileName p
  Noop  -> pure ()






{- NINE -}

-- | Recall our vector type:

data Vector (n :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | Imagine we want to write the '(!!)' function for this vector. If we wanted
-- to make this type-safe, and avoid 'Maybe', we'd have to have a type that can
-- only hold numbers /smaller/ than some type-level value.

-- | a. Implement this type! This might seem scary at first, but break it down
-- into Z and S cases. That's all the hint you need :)

data SmallerThan (limit :: Nat) where
  STZ :: SmallerThan ('S 'Z)
  STS :: SmallerThan n -> SmallerThan ('S n)

-- | b. Write the '(!!)' function:

(!!) :: Vector n a -> SmallerThan n -> a
(!!) (VCons a v) STZ = a
(!!) (VCons a v) (STS st) = v !! st

-- | c. Write a function that converts a @SmallerThan n@ into a 'Nat'.
stN :: SmallerThan n -> Nat
stN STZ = S Z
stN (STS s) = S $ stN s
