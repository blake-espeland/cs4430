module SubsetConstruction where

import GvShow
import qualified Data.List as List
import qualified Dfa
import qualified Nfa
import Nfa (Nfa(Nfa))
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
determinize (Nfa states inputs trans start final) =
  Dfa
    (sublists states)
    inputs
    transD
    (canonOrd start) {- the start state of the DFA is the set of all the NFA's start states -}
    [l | l <- sublists states , not (null (List.intersect l final)) ]
  where transD ss c = image (trans c) ss