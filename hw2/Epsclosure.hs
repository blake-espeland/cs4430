module Epsclosure where

import Relation
import Data.List
import Nfa
import qualified NfaEpsilon as Eps
import GvShow

data ABC = A | B | C
    deriving (Show , Eq , Ord , Enum)

data SixStates = S1 | S2 | S3 | S4 | S5 | S6
    deriving (Show , Eq , Ord , Enum)

instance GvShow ABC where
  gvshow = show

instance GvShow SixStates where
  gvshow = show

testEpsNfa :: Eps.Nfa ABC SixStates -- NFA shown in nfa.pdf
testEpsNfa = Eps.Nfa [S1, S2, S3, S4, S5, S6] [A, B, C] testTrans testEps [S1] [S4, S6]
    where 
        testTrans A = [(S1, S2)]
        testTrans B = [(S3, S4)]
        testTrans C = [(S5, S6)]
        testEps = [(S2, S3), (S2, S5)]


newTrans :: (Ord state) => [state] -> Rel state -> Trans input state -> Trans input state
newTrans states eps inTrans input = compose (rtClose states eps) (inTrans input)

newFinal :: Eq state => [state] -> [state] -> Rel state -> [state]
newFinal states final r = nub [x | (x, y) <- r, y `elem` final]

epsClosure :: Ord state => Eps.Nfa input state -> Nfa input state
epsClosure (Eps.Nfa states inputs trans eps start final) = 
    Nfa 
        states
        inputs 
        (newTrans states eps trans) -- new transition function
        start 
        (newFinal states final (rtClose states eps)) -- new final states


{-
(a)  The states of the new NFA are the same as the original one.
(b)  Compute  the  reflexive-transitive  closure  of  the eps relation  which  gives  the  epsilon-transitions.  There is a function in Relation.hs to compute this.
(c)  The  new  NFA’s  transition  relation  compose  that  closure  of eps with  the  the  original NFA’s transition relation, for each input letter.
(d)  The start states are the same.
(e)  The final states are all those states which can reach a final state of the original NFA, using  that  closure  of eps. I  used Data.List’s nub function  to  drop  duplicates  when computing this.
-}