module Regexp where

import GvShow
import NfaEpsilon
import SubsetConstructionEps
import qualified DfaOperations as Ops
import Dfa(Dfa)
import Util

data Regexp input =
   Empty
 | Char input
 | Or (Regexp input) (Regexp input) -- e + e'
 | Concat (Regexp input) (Regexp input)  -- e . e'
 | Star (Regexp input)  -- e * 
 deriving (Eq , Show)

data ToNfa = ToNfa1 | ToNfa2 | ToNfaL ToNfa | ToNfaR ToNfa 
  deriving (Eq , Ord)

instance Show ToNfa where
  show ToNfa1 = "1"
  show ToNfa2 = "2"
  show (ToNfaL x) = "L" ++ show x
  show (ToNfaR x) = "R" ++ show x  

instance GvShow ToNfa where
  gvshow = show
  
-- we assume both NFAs have the same set of inputs, for simplicity.
unionNfa :: Nfa input ToNfa -> Nfa input ToNfa -> Nfa input ToNfa
unionNfa n1 n2 = h (mapNfa ToNfaL n1) (mapNfa ToNfaR n2)
  where h (Nfa states inputs  trans eps start final)
          (Nfa states' _ trans' eps' start' final') =
         Nfa
          (states ++ states')
          inputs
          (\ c -> trans c ++ trans' c)
          (eps ++ eps')
          (start ++ start')
          (final ++ final')

concatNfa :: Nfa input ToNfa -> Nfa input ToNfa -> Nfa input ToNfa
concatNfa n1 n2 = h (mapNfa ToNfaL n1) (mapNfa ToNfaR n2)
  where h (Nfa states inputs trans eps start final)
          (Nfa states' _ trans' eps' start' final') =
         Nfa 
          (states ++ states')
          inputs
          (\ c -> trans c ++ trans' c)
          (eps ++ eps' ++ [(f,s) | f <- final, s <- start']) -- connect final to start' states
          start
          final' 

starNfa :: Nfa input ToNfa -> Nfa input ToNfa
starNfa n = h (mapNfa ToNfaR n)
  where h :: Nfa input ToNfa -> Nfa input ToNfa
        h (Nfa states inputs trans eps start final) =
         Nfa (ToNfa1 : states)
          inputs
          trans
          (map (\ st -> (ToNfa1,st)) start ++ map (\ st -> (st,ToNfa1)) final ++ eps) 
          [ToNfa1]
          [ToNfa1]


toNfah :: Eq input => [input] -> Regexp input -> Nfa input ToNfa
toNfah inputs Empty =
  Nfa [ToNfa1] inputs (\ _ -> []) [] [ToNfa1] []
toNfah inputs (Char c) =
  Nfa [ToNfa1,ToNfa2] inputs
      (\ c' -> if c == c' then [(ToNfa1,ToNfa2)] else [])
      [] [ToNfa1] [ToNfa2]
toNfah inputs (Or e1 e2) =
  unionNfa (toNfah inputs e1) (toNfah inputs e2)
toNfah inputs (Concat e1 e2) =
  concatNfa (toNfah inputs e1) (toNfah inputs e2)
toNfah inputs (Star e) =
  starNfa (toNfah inputs e)

-- collect the input letters used with Char constructors in the given regexp
collectInputsh :: Regexp input -> [input]
collectInputsh Empty = []
collectInputsh (Char c) = [c]
collectInputsh (Or r1 r2) = collectInputsh r1 ++ collectInputsh r2
collectInputsh (Concat r1 r2) = collectInputsh r1 ++ collectInputsh r2
collectInputsh (Star r) = collectInputsh r

collectInputs :: Ord input => Regexp input -> [input]
collectInputs = canonOrd . collectInputsh

toNfa :: Ord input => Regexp input -> Nfa input ToNfa
toNfa r = toNfah (collectInputs r) r

{- check whether or not a list of inputs matches a regexp, by converting to an NFA.
   We extract the input alphabet from the regexp.  The use of an NFA for checking
   acceptance actually allows the check to work correctly even it the input string
   contains some input letters that are not mentioned in the Regexp.  (This would
   not be true if we converted to a DFA first.) -}
checkRegexp :: Ord input => Regexp input -> [input] -> Bool
checkRegexp r s = accepts (toNfa r) s

toDfa :: Ord input => Regexp input -> Dfa input [[ToNfa]]
toDfa = Ops.minimize . determinize . toNfa 
