module Applic where

import Pda

{- Section 2 -}

{- Question 1 
    Example String 1: ( x x )
    Example String 2: x x x
-}

{- Question 2 
    E -> ( L )  :: -E, +), +L, +(
    E -> x      :: -E, +x
    L -> L E    :: -L, +E, +L
    L -> E E    :: -L, +E, +E
-}
data SEL = Start | E | L
  deriving (Eq,Ord,Show)

funcall :: Pda Char Int SEL
funcall = Pda [0..15] ['(', 'x', ')'] [Start, E, L] trans eps 0 [15]
    where eps = [(0, Push Start, 1), (2, Pop Start, 15), (6, Noop, 2), (8, Noop, 2), (11, Noop, 2), (14, Noop, 2)]
          trans '(' = [(2, Push E, 2), (2, Push L, 2)]
          trans ')' = [(2, Pop E, 2)]
          trans 'x' = [(2, Pop E, 2)]
          trans L = [(2, Noop, 2)]
