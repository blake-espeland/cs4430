module Applic where

import Pda

{- Section 2 -}

{- Question 1 
    Example String 1: ( x x )
    Example String 2: ( x x ( x x x ) ( x x ) )
-}

{- Question 2 
    E -> ( L )  :: -E, +), +L, +(
    E -> x      :: -E, +x
    L -> L E    :: -L, +E, +L
    L -> E E    :: -L, +E, +E
-}

data BP = Start | P | X
  deriving (Eq,Ord,Show)

funcall :: Pda Char Int BP
funcall = Pda [0..7] ['(', 'x', ')'] [Start, P, X] trans eps 0 [7]
  where eps = [(0, Push Start, 1), (1, Push X, 2), (4, Push P, 5), (6, Push X, 4), (3, Pop Start, 7), (5, Pop Start, 7)]
        trans '(' = [(2, Push X, 4), (5, Push X, 6), (5, Push X, 6)]
        trans 'x' = [(2, Pop X, 3), (5, Pop X, 5)]
        trans ')' = [(5, Pop P, 5)]

test = accepts funcall "(xx)" -- should be True
test2 = accepts funcall "()" -- should be False