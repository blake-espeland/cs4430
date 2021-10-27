{- code for implementing nondeterministic finite automata (NFA) following
   the approach presented in class (different from Kozen's) -}
module Nfa where

import GvShow
import Relation

{- the type for transition functions: given an input,
   return the relation corresponding to edges labeled with that
   input. -}
type Trans input state = input -> Rel state

data Nfa input state =
  Nfa {
    states :: [state], -- Q: all states.  
    inputs :: [input], -- Sigma: all possible inputs.
    trans :: Trans input state, -- R: transition relations
    start :: [state], -- start states
    final :: [state]} -- final states
    
{- given a NFA, return the multistep transition function,
   which returns the list of states you reach when processing
   a sequence of inputs from a given starting state -}
multistep :: Eq state => Nfa input state -> Trans [input] state
multistep n@(Nfa states _ _ _ _) [] = identity states
multistep n@(Nfa states _ trans _ _) (c:cs) =
  compose (trans c) (multistep n cs)

accepts :: Eq state => Nfa input state -> [input] -> Bool
accepts n@(Nfa _ _ trans start final) str =
  not ( null ( intersect (multistep n str) [(s,f) | s <- start, f <- final]))

----------------------------------------------------------------------
-- to GraphViz format

foldrGlue f xs str = foldr f str xs

-- this assumes distinct states and inputs are shown distinctly
toGraphViz :: (GvShow input, GvShow state, Eq state) => Nfa input state -> String
toGraphViz (Nfa states inputs trans start finals) =
    "digraph nfa {\n" ++
    "rankdir = LR;\n" ++
    "hidden [shape = plaintext, label = \"\"];\n" ++
    "node [shape = doublecircle];\n" ++
    (foldrGlue (\ f str -> 
                   gvshow f ++ " [label = \"\"];\n" ++ str)
               finals
       ("node [shape = point];\n" ++
       foldrGlue (\ start str ->
         "hidden -> " ++ gvshow start ++ ";\n" ++ str)
         start
       -- loop over states st
       (foldrGlue (\ st str ->
                -- loop over input letters c
                foldrGlue (\ c str ->
                         -- loop over states with an edge labeled c from st
                         foldrGlue (\ st' str ->
                                  gvshow st ++ " -> " ++ gvshow st' ++ " [label = \"" ++ gvshow c ++ "\"];\n" ++ str)
                               [s' | (s,s') <- (trans c), s == st] 
                               str)
                      inputs
                      str)
         states
         "}\n")))

-- write the given NFA in GraphViz format to the file named filename.
writeGraphViz :: (GvShow input, GvShow state, Eq state) => String -> Nfa input state -> IO ()
writeGraphViz filename d =
  writeFile filename (toGraphViz d)