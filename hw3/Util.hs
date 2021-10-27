module Util where

import Data.List

-- canonize a list of ordered elements
canonOrd :: Ord a => [a] -> [a] 
canonOrd = sort . nub

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint x f =
  let x' = f x in
    if x == x' then
      x'
    else
      fixedPoint x' f

foldrGlue f xs str = foldr f str xs

      