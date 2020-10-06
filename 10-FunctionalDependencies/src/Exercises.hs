{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

import Data.Kind (Type)
import Data.Void (Void)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(Text))
import GHC.Generics (Generic (..))
import qualified GHC.Generics as G





{- ONE -}

-- | Recall an old friend, the 'Newtype' class:

class Newtype (new :: Type) (old :: Type) | new -> old where
  wrap   :: old -> new
  unwrap :: new -> old

-- | a. Can we add a functional dependency to this class?

-- yes

-- | b. Why can't we add two?

-- newtypes wrap existing types; there are more newtypes than newtyped-types,
-- and so a old -> new fundep is overly restrictive




{- TWO -}

-- | Let's go back to a problem we had in the last exercise, and imagine a very
-- simple cache in IO. Uncomment the following:

class (Functor f) => CanCache (entity :: Type) (index :: Type) (f :: Type -> Type) | entity -> index where
  store :: entity -> f ()
  load  :: index -> f (Maybe entity)

-- | a. Uh oh - there's already a problem! Any @entity@ type should have a
-- fixed type of id/@index@, though... if only we could convince GHC... Could
-- you have a go?

-- | b. @IO@ is fine, but it would be nice if we could choose the functor when
-- we call @store@ or @load@... can we parameterise it in some way?

-- | c. Is there any sort of functional dependency that relates our
-- parameterised functor to @entity@ or @index@? If so, how? If not, why not?
--
-- No. Conceptually the cache is independent from what's being stored. IO in
-- production vs Map in testing or instanx=ce. But also, because the functor is
-- used in both methods, it doesn't need to be parameterized for ghc to
-- understand it.





{- THREE -}

-- | Let's re-introduce one of our old favourites:
data Nat = Z | S Nat

-- | When we did our chapter on @TypeFamilies@, we wrote an @Add@ family to add
-- two type-level naturals together. If we do a side-by-side comparison of the
-- equivalent "class-based" approach:

class       Add  (x :: Nat) (y :: Nat) (z :: Nat) | x y -> z
type family Add' (x :: Nat) (y :: Nat)    :: Nat

-- | We see here that there are parallels between classes and type families.
-- Type families produce a result, not a constraint, though we could write
-- @Add' x y ~ z => ...@ to mean the same thing as @Add x y z => ...@. Also,
-- the result of a type family is determined by its inputs - something we can
-- express as a functional dependency!

-- | a. Write the two required instances for the 'Add' class by
-- pattern-matching on the first argument. Remember that instances can have
-- constraints, and this is how we do recursion!

instance Add 'Z n n
instance (Add m n z) => Add ('S m) n ('S z)

-- | b. By our analogy, a type family has only "one functional dependency" -
-- all its inputs to its one output. Can we write _more_ functional
-- dependencies for @Add@? Aside from @x y -> z@? 

-- x z -> y and y z -> should also be valid

-- | c. We know with addition, @x + y = z@ implies @y + x = z@ and @z - x = y@.
-- This should mean that any pair of these three variables should determine the
-- other! Why couldn't we write all the possible functional dependencies that
-- /should/ make sense?

-- we'd have to prove all those things about addition first



{- FOUR -}

data Proxy (a :: k) = Proxy

-- | As we all know, type signatures are /not/ documentation. This is really
-- because the names of types are far too confusing. To that end, we can give
-- our types friendlier names to make the coding experience less intimidating:

class (x :: k) `IsNamed` (label :: Symbol) | x -> label, label -> x where
  fromName :: Proxy x     -> Proxy label
  fromName _ = Proxy

  toName :: Proxy label -> Proxy x
  toName _ = Proxy

-- | Now we have this class, we can get to work!

instance Int   `IsNamed` "Dylan"
instance IO    `IsNamed` "Barbara"
instance Float `IsNamed` "Kenneth"
-- instance Float `IsNamed` "Bob"
-- instance String `IsNamed` "Dylan"

-- | a. In our glorious new utopia, we decide to enact a law that says, "No two
-- types shall have the same name". Similarly, "No type shall have two names".
-- Is there a way to get GHC to help us uphold the law?

-- | b. Write the identity function restricted to types named "Kenneth".

kenneth :: (a `IsNamed` "Kenneth") => a -> a
kenneth = id

-- | c. Can you think of a less-contrived reason why labelling certain types
-- might be useful in real-world code?

-- it could be used as a friendly message in type errors



{- FIVE -}

-- | Here's a fun little class:
class Omnipresent (r :: Symbol) (s :: Symbol) | -> r, -> s

-- | Here's a fun little instance:
instance Omnipresent "Tom!" "Bob"
-- instance Omnipresent "Bob"

-- | a. Is there a way to enforce that no other instance of this class can ever
-- exist? Do we /need/ variables on the left-hand side of a functional
-- dependency arrow?

-- | b. Can you think of a time you would ever want this guarantee? Is this
-- "trick" something you can think of a practical reason for doing? Perhaps if
-- we added a method to the class? (Very much an open question).

-- | c. Add another similarly-omnipresent parameter to this type class.





{- SIX -}

-- | You knew it was coming, didn't you?

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

data SNat (n :: Nat) where
  SZ ::           SNat  'Z
  SS :: SNat n -> SNat ('S n)

-- | a. Write a function (probably in a class) that takes an 'SNat' and an
-- 'HList', and returns the value at the 'SNat''s index within the 'HList'.

class Hat (n :: Nat) (xs :: [Type]) (at :: Type) | n xs -> at where
  getAt :: SNat n -> HList xs -> at

instance (Hat n xs res) => Hat ('S n) (y ': xs) res where
  getAt (SS n) (HCons _ xs) = getAt n xs

instance Hat 'Z (x ': xs) x where
  getAt SZ (HCons x _) = x

instance (TypeError (Text "type not in list")) => Hat n '[] Void where
  getAt = error "can't get here"

-- | b. Add the appropriate functional dependency.

-- | c. Write a custom type error!

-- | d. Implement 'take' for the 'HList'.





{- SEVEN -}

-- | Recall our variant type:

data Variant (xs :: [Type]) where
  Here  ::         x  -> Variant (x ': xs)
  There :: Variant xs -> Variant (y ': xs)

-- | We previously wrote a function to "inject" a value into a variant:

class Inject (x :: Type) (xs :: [Type]) where
  inject :: x -> Variant xs

instance Inject x (x ': xs) where
  inject = Here

instance {-# OVERLAPPING #-} Inject x xs
    => Inject x (y ': xs) where
  inject = There . inject

-- | Write a function to "project" a value /out of/ a variant. In other words,
-- I would like a function that takes a proxy of a type, a variant containing
-- that type, and returns /either/ a value of that type /or/ the variant
-- /excluding/ that type:
--
-- @
-- test :: Bool
-- test =
--   project @Bool (inject True :: Variant '[Int, String, Bool])
--     === Left Bool :: Either Bool (Variant '[Int, String])
-- @

class Project (x :: Type) (xs :: [Type]) where
  project :: Variant xs -> Either x (Variant xs)

instance Project x (x ': xs) where
  project (Here v) = Left v

instance (Project x xs) => Project x (Variant y ': xs) where
  project (There v) =
    case project v of
      Right v' -> Right $ There v'
      Left y -> Left y





{- EIGHT -}

-- | It would be nice if I could update a particular index of an HList by
-- providing an index and a (possibly-type-changing) function. For example:
--
-- @
--   update SZ length (HCons True (HCons "Hello" HNil))
--     === HCons True (HCons 5 HNil)
-- @

-- | Write the type class required to implement this function, along with all
-- its instances and functional dependencies.

class Update (n :: Nat) (x :: Type) (y :: Type) (xs :: [Type]) (ys :: [Type]) | x y xs -> ys where
  update :: SNat n -> (x -> y) -> HList xs -> HList ys

instance Update 'Z x y (x ': xs) (y ': xs) where
  update SZ f (HCons x xs) = HCons (f x) xs

instance (Update n x y xs ys) => Update ('S n) x y (z ': xs) (z ': ys) where
  update (SS n) f (HCons z xs) = HCons z $ update n f xs

{- NINE -}

-- | If you've made it this far, you're more than capable of digesting and
-- understanding some advanced GHC docs! Read the documentation at
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html, and
-- keep going until you hit 'Generic1' - we won't worry about that today.

-- | We can write a little function to get the name of a type as a type-level
-- symbol like so:

class NameOf (x :: Type) (name :: Symbol) | x -> name
instance GNameOf (Rep x) name => NameOf x name

-- | We then have to implement this class that examines the generic tree...
class GNameOf (rep :: Type -> Type) (name :: Symbol) | rep -> name
instance GNameOf (G.D1 ('G.MetaData name a b c) d) name

-- | Write a function to get the names of the constructors of a type as a
-- type-level list of symbols.





{- TEN -}

-- In the standard library, we have a series of @liftA*@ functions, such as
-- 'liftA2', 'liftA3', 'liftA4'... wouldn't it be nice if we just had /one/
-- function called 'lift' that generalised all these?
--
liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 = lift

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 = lift


liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 = lift

-- Write this function, essentially generalising the f <$> a <*> b <*> c...
-- pattern. It may help to see it as pure f <*> a <*> b <*> c..., and start
-- with a function like this:

-- lift :: (Applicative f, Lift f i o) => i -> o
-- lift = lift' . pure

-- @class Lift f i o ... where lift' :: ...@ is your job! If you get this
-- right, perhaps with some careful use of @INCOHERENT@, equality constraints,
-- and functional dependencies, you should be able to get some pretty amazing
-- type inference:
--
-- >>> :t lift (++)
-- lift (++) :: Applicative f => f [a] -> f [a] -> f [a]

lift :: forall f i o. (Applicative f, Lift f i o) => i -> o
lift = lift' . pure @f

class Lift (f :: Type -> Type) -- Applicative in question
           i -- Function to be lifted a -> b -> c -> d ...
           o -- lifted function f a -> f b -> f c -> f d
  where
  lift' :: f i -> o

instance {-# OVERLAPPING #-} (Applicative f, Lift f b c, f ~ f') => Lift f (a -> b) (f' a -> c) where
  lift' fab = lift' . (fab <*>)

instance {-# INCOHERENT #-} (Applicative f, f ~ f') => Lift f a (f' a) where
  lift' fa = fa
