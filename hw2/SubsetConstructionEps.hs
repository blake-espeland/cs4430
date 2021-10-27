module SubsetConstructionEps where

import GvShow
import qualified Data.List as List
import qualified Dfa
import qualified Nfa
import NfaEpsilon (Nfa(Nfa))
import Dfa (Dfa(Dfa))
import Data.List
import Util
import Relation

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) =
  let r = sublists xs in
    r ++ map (x:) r

determinize :: Ord state => Nfa input state -> Dfa input [state]
determinize (Nfa states inputs trans eps start final) =
  let eps' = rtClose states eps in
  let transD ss c = canonOrd $ image eps' $ image (compose eps' (trans c)) ss in
   Dfa
    (nub $ map (canonOrd . image eps') $ sublists states)
    inputs
    transD
    (canonOrd $ image eps' start) {- the start state of the DFA is the set of
                            all the NFA's start states, followed by epsilon transitions -}
    (nub [canonOrd $ image eps' l | l <- sublists states , not (null (List.intersect l final)) ])
