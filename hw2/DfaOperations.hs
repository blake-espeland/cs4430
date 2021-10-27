module DfaOperations where

import Data.List 
import Dfa
import Relation
import Util

{- return a DFA whose language is the complement of the language
   of the input DFA. -}
complement :: Eq state => Dfa input state -> Dfa input state
complement (Dfa states inputs trans start final) =
  (Dfa states inputs trans start (states \\ final))

prod :: [state1] -> [state2] -> [(state1, state2)]
prod states1 states2 = [(s, s') | s <- states1 , s' <- states2]

prodTrans :: Trans input state1 -> Trans input state2 -> Trans input (state1, state2)
prodTrans trans1 trans2 (s, s') c = (trans1 s c,trans2 s' c)

union :: Dfa input state1 -> Dfa input state2 -> Dfa input (state1, state2)
union (Dfa states1 inputs trans1 start1 final1) (Dfa states2 _ trans2 start2 final2) =
  Dfa (prod states1 states2) inputs (prodTrans trans1 trans2) (start1, start2)
    ([ (s, s') | s <- final1, s' <- states2] ++ [(s, s') | s <- states1 , s' <- final2])

intersect :: Dfa input state1 -> Dfa input state2 -> Dfa input (state1, state2)
intersect (Dfa states1 inputs trans1 start1 final1) (Dfa states2 _ trans2 start2 final2) =
  Dfa (prod states1 states2) inputs (prodTrans trans1 trans2) (start1, start2) (prod final1 final2)

{- equiv should be an equivalence relation respected by the DFA (if you want
   the languages to be the same)
-}
quotient :: Ord state => Dfa input state -> Rel state -> Dfa input [state]
quotient (Dfa states inputs trans start final) equiv =
  Dfa (canonOrd [ec equiv q | q <- states])
    inputs
    trans'
    (ec equiv start)
    (canonOrd [ec equiv f | f <- final])
  where trans' ss a = canonOrd $ concat $ [ ec equiv (trans s a) | s <- ss ] -- if trans respects equiv, this will be in the set of states

-- compute the coarsest equivalence relation respected by the given DFA
coarsest :: Ord state => Dfa input state -> Rel state
coarsest (Dfa states inputs trans start final) =
  let nonfinal = states \\ final in
  let init = symmClose [ (s1,s2) | s1 <- nonfinal , s2 <- final ] in
  let step forbidden = canonOrd (Relation.union forbidden
                        [ (s1,s2) | s1 <- states ,
                                    s2 <- states ,
                                    a <- inputs ,
                                    elem (trans s1 a, trans s2 a) forbidden ]) in
    prod states states \\ fixedPoint init step

dropUnreachable :: Ord state => Dfa input state -> Dfa input state
dropUnreachable (Dfa states inputs trans start final) =
  let reachable =
        fixedPoint [start]
          (\ ss -> canonOrd $ ss ++ [ trans s a | a <- inputs , s <- ss ]) 
  in
    (Dfa reachable inputs trans start (Data.List.intersect final reachable))

minimize :: Ord state => Dfa input state -> Dfa input [state]
minimize d =
  let d' = dropUnreachable d in
    quotient d' (coarsest d')