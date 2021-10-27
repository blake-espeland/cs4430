module Sol where

import Regexp
import SubsetConstructionEps
import qualified DfaOperations as Ops
import Dfa
import qualified NfaEpsilon as Eps
import Relation
import Data.List
import Nfa


{- Problem 1, Writing regular expressions

   I am using Char as the type for inputs.

   Replace the 'undefined's below with values of type Regexp Char (see Regexp.hs).
-}

re1 :: Regexp Char -- Mystery 3
re1 = Or 
         (Concat 
            (Char 'A') 
            (Star (Char 'B'))) 
         (Concat 
            (Char 'B') 
            (Star (Char 'A')))

re2 :: Regexp Char -- Mystery 1
re2 = Star 
         (Concat 
            (Char 'A') 
            (Concat 
               (Char 'B') 
               (Concat 
                  (Star (Char 'B')) 
                  (Char 'C'))))

re3 :: Regexp Char -- Mystery 4
re3 = Concat 
         (Star 
            (Or 
               (Char 'A') 
               (Char 'B')))  
         (Concat 
            (Char 'B') 
            (Concat 
               (Char 'B') 
               (Star (Char 'C'))))

re4 :: Regexp Char -- Mystery 5
re4 = Concat 
         (Or 
            (Star (Char 'A'))
            (Concat
               (Char 'B')
               (Star (Char 'B'))))
         (Star 
            (Or
               (Char 'A')
               (Char 'C')))

re5 :: Regexp Char -- Mystery 2
re5 = Star
         (Concat
            (Star 
               (Or 
                  (Char 'B')
                  (Char 'C')))
            (Char 'A'))


{-
Section 2 Question 2:
The shown DFA is an intersection of re2 and re3.
The accepted regex's are of the form: ABBB*C.
-}
