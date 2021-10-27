module PdaExamples where

import Pda

data BP = Start | Step
  deriving (Eq,Ord,Show)

ab :: Pda Char Int BP
ab = Pda [0..3] ['a','b'] [Start,Step] trans eps 0 [3]
  where eps = [(0,Push Start,1), (1,Noop,2), (2,Pop Start,3)]
        trans 'a' = [(1,Push Step,1)]
        trans 'b' = [(2,Pop Step,2)]

test = multistep ab "aaabbb"


{-

-}

balparens :: Pda Char Int BP
balparens = Pda [0..2] ['(',')'] [Start,Step] trans eps 0 [2]
  where eps = [(0,Push Start,1), (1,Pop Start,2)]
        trans '(' = [(1,Push Step,1)]
        trans ')' = [(1,Pop Step,1)]

test2 = accepts balparens "(()(()))"