{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE RankNTypes     #-}
module Exercises where

import Data.Kind (Type)





{- ONE -}

-- | The following GADT creates a list of values of unknown types:

data Exlistential where
  Nil  :: Exlistential
  Cons :: a -> Exlistential -> Exlistential

-- | a. Write a function to "unpack" this exlistential into a list.

unpackExlistential :: Exlistential -> (forall a. a -> r) -> [r]
unpackExlistential Nil _ = []
unpackExlistential (Cons a exl) f = f a : unpackExlistential exl f

-- | b. Regardless of which type @r@ actually is, what can we say about the
-- values in the resulting list?

-- that there's one of them for every element in the exlistential

-- | c. How do we "get back" knowledge about what's in the list? Can we?

-- No, we can't





{- TWO -}

-- | Consider the following GADT that existentialises a 'Foldable' structure
-- (but, crucially, not the type inside).

data CanFold a where
  CanFold :: Foldable f => f a -> CanFold a

-- | a. The following function unpacks a 'CanFold'. What is its type?

unpackCanFold :: (forall m. Foldable m => m a -> b) -> CanFold a -> b
unpackCanFold f (CanFold x) = f x

-- | b. Can we use 'unpackCanFold' to figure out if a 'CanFold' is "empty"?
-- Could we write @length :: CanFold a -> Int@? If so, write it!

cfLength :: CanFold a -> Int
cfLength = unpackCanFold length

-- | c. Write a 'Foldable' instance for 'CanFold'. Don't overthink it.

instance Foldable CanFold where
  foldMap f = unpackCanFold (foldMap f)




{- THREE -}

-- | Recall our existential 'EqPair' GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. Write a function that "unpacks" an 'EqPair' by applying a user-supplied
-- function to its pair of values in the existential type.

unEq :: (forall a. a -> a -> b) -> EqPair -> b
unEq f (EqPair x y) = f x y

-- | b. Write a function that takes a list of 'EqPair's and filters it
-- according to some predicate on the unpacked values.

filterEq :: (forall a. a -> a -> Bool) -> [EqPair] -> [EqPair]
filterEq f = filter (unEq f)

-- | c. Write a function that unpacks /two/ 'EqPair's. Now that both our
-- variables are in rank-2 position, can we compare values from different
-- pairs?

unEq2 :: (forall a.  Eq a => a -> a -> b) -> EqPair -> EqPair -> (b, b)
unEq2 f (EqPair a b) (EqPair c d) = (f a b, f c d)

twoEqPairEq :: EqPair -> EqPair -> Bool
twoEqPairEq p1 p2 = uncurry (==) $ unEq2 (==) p1 p2




{- FOUR -}

-- | When I was building @purescript-panda@, I came across a neat use case for
-- rank-2 types. Consider the following sketch of a type:

data Component input output
  -- = Some sort of component stuff.

-- | Now, let's imagine we want to add a constructor to "nest" a component
-- inside another component type. We need a way of transforming between our
-- "parent" I/O and "child" I/O, so we write this type:

data Nested input output subinput suboutput
  = Nested
      { inner  :: Component subinput suboutput
      , input  :: input -> subinput
      , output :: suboutput -> output
      }

-- | a. Write a GADT to existentialise @subinput@ and @suboutput@.

data NestedX i o where
  Base :: Component i o -> NestedX i o
  Nest :: NestedX si so -> (i -> si) -> (so -> o) -> NestedX i o

-- | b. Write a function to "unpack" a NestedX. The user is going to have to
-- deal with all possible @subinput@ and @suboutput@ types.

unNestX :: (forall i o. Component i o -> i -> o) -> NestedX i o -> i -> o
unNestX run (Base c) =  run c
unNestX run (Nest cs fi fo) = fo . unNestX run cs . fi

-- | c. Why might we want to existentialise the subtypes away? What do we lose
-- by doing so? What do we gain?

-- force 

-- In case you're interested in where this actually turned up in the code:
-- https://github.com/i-am-tom/purescript-panda/blob/master/src/Panda/Internal/Types.purs#L84





{- FIVE -}

-- | Let's continue with the theme of the last question. Let's say I have a few
-- HTML-renderable components:

data FirstGo input output
  = FText String
  | FHTML (String, String) [FirstGo input output]
  --       ^ properties     ^ children

-- | This is fine, but there's an issue: some functions only really apply to
-- 'FText' /or/ 'FHTML'. Now that this is a sum type, they'd have to result in
-- a 'Maybe'! Let's avoid this by splitting this sum type into separate types:

data Text = Text String
data HTML = HTML { properties :: (String, String), children :: Children }

-- | Uh oh! What's the type of our children? It could be either! In fact, it
-- could probably be anything that implements the following class, allowing us
-- to render our DSL to an HTML string:
class Renderable component where render :: component -> String

-- | a. Write a type for the children.
--
data Children where
  Only :: (Renderable c) => c -> Children
  Child :: (Renderable c) => c -> Children -> Children


-- | b. What I'd really like to do when rendering is 'fmap' over the children
-- with 'render'; what's stopping me? Fix it!

-- lots of things. 
-- 1) Children isn't polymorphic. Nor can it be, because of the renderable
-- constraint
-- 2) HTML isn't polymorphic; children are exactly `Children`, not `a`

data HTMLp a = HTMLp { pproperties :: (String, String), pchildren :: a }

instance Renderable Children where
  render (Only c) = render c
  render (Child c cs) = render c <> render cs

instance Functor HTMLp where
  fmap f (HTMLp ps c) = HTMLp ps (f c)

-- | c. Now that we're an established Haskell shop, we would /also/ like the
-- option to render our HTML to a Shakespeare template to write to a file
-- (http://hackage.haskell.org/package/shakespeare). How could we support this
-- new requirement with minimal code changes?

-- add renderToShakespear to the `Renderable` class.




{- SIX -}

-- | Remember our good ol' mystery box?

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | a. Knowing what we now know about RankNTypes, we can write an 'unwrap'
-- function! Write the function, and don't be too upset if we need a 'Maybe'.

unwrapMystery :: (forall a. MysteryBox a -> r) -> MysteryBox a -> Maybe r
unwrapMystery _ EmptyBox = Nothing
unwrapMystery f (IntBox _ b) = Just $ f b
unwrapMystery f (StringBox _ b) = Just $ f b
unwrapMystery f (BoolBox _ b) = Just $ f b

-- | b. Why do we need a 'Maybe'? What can we still not know?

-- Whether there's anything inside the box at all

-- | c. Write a function that uses 'unwrap' to print the name of the next
-- layer's constructor.

nextConstructor :: MysteryBox a -> Maybe String
nextConstructor = unwrapMystery $ \case
  EmptyBox -> "EmptyBox"
  IntBox _ _ -> "IntBox"
  StringBox _ _ -> "StringBox"
  BoolBox _ _ -> "BoolBox"


{- SEVEN -}

-- | When we talked about @DataKinds@, we briefly looked at the 'SNat' type:

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | We also saw that we could convert from an 'SNat' to a 'Nat':

toNat :: SNat n -> Nat
toNat SZ = Z
toNat (SS n) = S $ toNat n

-- | How do we go the other way, though? How do we turn a 'Nat' into an 'SNat'?
-- In the general case, this is impossible: the 'Nat' could be calculated from
-- some user input, so we have no way of knowing what the 'SNat' type would be.
-- However, if we could have a function that would work /for all/ 'SNat'
-- values...

-- | Implement the 'fromNat' function. It should take a 'Nat', along with some
-- SNat-accepting function (maybe at a higher rank?) that returns an @r@, and
-- then returns an @r@. The successor case is a bit weird here - type holes
-- will help you!

fromNat :: (forall n. SNat n -> r) -> Nat -> r
fromNat f Z = f SZ
fromNat f (S n) = fromNat (\sn -> f (SS sn)) n

-- | If you're looking for a property that you could use to test your function,
-- remember that @fromNat x toNat === x@!





{- EIGHT -}

-- | Bringing our vector type back once again:

data Vector (n :: Nat) (a :: Type) where
  VNil  ::                    Vector  'Z    a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | It would be nice to have a 'filter' function for vectors, but there's a
-- problem: we don't know at compile time what the new length of our vector
-- will be... but has that ever stopped us? Make it so!

filterV :: (forall n. Vector n a -> r) -> (a -> Bool) -> Vector m a -> r
filterV k p VNil = k VNil
filterV k p (VCons a vs) =
  if p a
    then filterV (k . VCons a) p vs
    else filterV k p vs
