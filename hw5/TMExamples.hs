module TMExamples where

import TM

-- following suggestion by Junnan in class
tripletm =
  TM [1 .. 6] "abc" "abc*! " id ' ' '!' trans 1 [6]
  where
    trans = goRight 1 ' ' ' ' 6 ++
            loopRight 1 "*" ++
            goRight 1 'a' '*' 2 ++
            loopRight 2 "a*" ++
            goRight 2 'b' '*' 3 ++
            loopRight 3 "b*" ++
            goRight 3 'c' '*' 4 ++
            loopRight 4 "c*" ++
            goLeft 4 ' ' ' ' 5 ++
            loopLeft 5 "abc*" ++
            goRight 5 '!' '!' 1 
testtripletm = accepts tripletm "aaabbbccc"

quadtm = 
  TM [1 .. 7] "abcd" "abcd*! " id ' ' '!' trans 1 [7]
  where
    trans = goRight 1 ' ' ' ' 7 ++
            loopRight 1 "*" ++
            goRight 1 'a' '*' 2 ++
            loopRight 2 "a*" ++
            goRight 2 'b' '*' 3 ++
            loopRight 3 "b*" ++
            goRight 3 'c' '*' 4 ++
            loopRight 4 "c*" ++
            goRight 4 'd' '*' 5 ++
            loopRight 5 "d*" ++
            goLeft 5 ' ' ' ' 6 ++
            loopLeft 6 "abcd*" ++
            goRight 6 '!' '!' 1 

testquadtm1 = accepts quadtm "aaabbbcccddd"
testquadtm2 = accepts quadtm "aba"


{- 
{w b w | w ∈ {a, c}∗} 
Examples:
accaacabaccaaca
aba
cacbcac

-}
eqtm =
  TM [1 .. 9] "abc" "abc*! " id ' ' '!' trans 1 [9]
  where
    trans = goRight 1 ' ' ' ' 9 ++
            loopRight 1 "b*" ++
            -- Detecting a's on both sides
            goRight 1 'a' '*' 2 ++
            loopRight 2 "ac*" ++
            goRight 2 'b' 'b' 3 ++
            loopRight 3 "*" ++
            goRight 3 'a' '*' 4 ++
            loopRight 4 "ac*" ++
            goLeft 4 ' ' ' ' 8 ++
            -- Detecting c's on both sides
            goRight 1 'c' '*' 5 ++
            loopRight 5 "ac*" ++
            goRight 5 'b' 'b' 6 ++
            loopRight 6 "*" ++
            goRight 6 'c' '*' 7 ++
            loopRight 7 "ac*" ++
            goLeft 7 ' ' ' ' 8 ++
            -- Looping back
            loopLeft 8 "abc*" ++
            goRight 8 '!' '!' 1

testeqtm1 = accepts eqtm "aaabbbccc"
testeqtm2 = accepts eqtm "accbacc"
testeqtm3 = accepts eqtm "accabacca"
