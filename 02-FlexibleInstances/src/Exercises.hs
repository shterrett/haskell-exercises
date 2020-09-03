module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

-- instance PopQuiz Bool
-- instance PopQuiz [Bool] -- this one does!
-- instance PopQuiz [a]
-- instance PopQuiz (a, b)
-- instance PopQuiz [(a, b)] -- this one does!
-- instance PopQuiz (IO a)

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a) -- this one does because Int isn't a variable
-- instance PopQuiz (RIO r a)
-- instance PopQuiz (RIO' r a) -- this one does, because type synonyms
-- instance PopQuiz (r -> IO a) -- again with the nested type constructors
-- instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b). -- this is fine
-- instance PopQuiz (a -> b -> c) -- the second (->) is nested, so this one does
-- instance PopQuiz (a, b, c)
-- instance PopQuiz (a, (b, c)) -- this one does; nested type constructors
-- instance PopQuiz ()
-- instance PopQuiz (a, b, c, a) -- this one does; repeated variables

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a) -- this one does; repeated variables
-- instance PopQuiz (Pair a) -- this one does not
-- instance PopQuiz (Pair' a) -- this one does because type synonyms (and then repeated type variables
