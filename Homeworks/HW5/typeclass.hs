import Data.List

data PairRL a b = P a b

instance (Show a, Show b) => Show (PairRL a b) where
  show (P t1 t2) = "(" ++ (show t1) ++ "," ++ (show t2) ++ ")"

{- Define the following two functions to define equality and ordering on
 our reverse lexicographic pairs. These declarations can be read:

   "Given a way to determine equality for values of type a and type b,
    equality on PairRL a b is given by..."

where the current rules are not yet written. A similar reading holds for Ord -}

{- instance (Eq a, Eq b) => Eq (PairRL a b) where ... -}

{- instance (Ord a, Ord b) => Ord (PairRL a b) where ... -}


main :: IO ()
main = undefined
{- Delete undefined above, and uncomment the following after completing
the two typeclasses. Without those implementations, the
following fails to compile because Haskell doesn't know how to compare
values of the PairRL type. -}
{-
  let a = [P 1 2, P 3 5, P 6 11, P 3 1,P 1 1] in
    putStrLn (show (sort a))
-}
