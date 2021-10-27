{- code for implementing deterministic finite automata (DFA) -}
module Dfa where

import GvShow
import qualified Data.List as List

{- the type for transition functions: given a starting state and an input,
   return the next state -}
type Trans input state = (state -> input -> state)

{- representing a DFA whose input alphabet is given by *input*, and
   whose state space is given by *state* -}
data Dfa input state =
  Dfa { 
    states :: [state], -- Q: all states.  Code later assumes this is the set of reachable states
    inputs :: [input], -- Sigma: all possible inputs.  This should be the extension of input
    trans :: (Trans input state), -- delta, the transition function
    start :: state, -- s, start state
    final :: [state]} -- F, final states 

{- given a DFA, return the multistep transition function,
   which returns the state you reach when processing a sequence of
   inputs from a given starting state -}
multistep :: Dfa input state -> Trans [input] {- Sigma* -} state
multistep (Dfa _ _ delta _ _) = deltahat
  where deltahat q [] = q
        deltahat q (c:cs) = deltahat (delta q c) cs

-- an alternative definition using the foldl function
multistep' :: Dfa input state -> Trans [input] state
multistep' (Dfa _ _ trans _ _) = foldl trans

accepts :: Eq state => Dfa input state -> [input] -> Bool
accepts d@(Dfa _ _ trans start final) str =
  List.elem (multistep d start str) final

----------------------------------------------------------------------
-- showing a DFA (converting it to a string)

instance (Show input , Show state) => Show (Dfa input state) where
  show (Dfa states alphabet trans start final) =
    "States: " ++ show states ++ "\n" ++
    "Alphabet: " ++ show alphabet ++ "\n" ++
    "Transitions:\n" ++
       concat (map (\ (s,a) ->
                      "  " ++ show s ++ "," ++ show a ++ " -> " ++
                      show (trans s a) ++ "\n")
                [(s,a) | s <- states, a <- alphabet]) ++ 
    "Start state: " ++ show start ++ "\n" ++
    "Final states: " ++ show final



----------------------------------------------------------------------
-- runs: these are sequences alternating between states and inputs,
-- which show how the automata operates on an input string

data Run input state = Run [(state,input)] state

instance (Show input , Show state) => Show (Run input state) where
  show (Run r f) =
    foldr (\ (st,c) str -> show st ++ " --" ++ show c ++ "--> " ++ str) (show f) r

runh :: Trans input state -> state -> [input] -> Run input state
runh trans st [] = Run [] st
runh trans st (c:cs) =
  let (Run r f) = runh trans (trans st c) cs in
    Run ((st,c) : r) f

run :: Dfa input state -> [input] -> Run input state
run (Dfa _ _ trans start _) cs = runh trans start cs

----------------------------------------------------------------------
-- to GraphViz format

foldrGlue f xs str = foldr f str xs

-- this assumes distinct states and inputs are shown distinctly
toGraphViz :: (GvShow input, GvShow state) => Dfa input state -> String
toGraphViz (Dfa states inputs trans start finals) =
    "digraph dfa {\n" ++
    "rankdir = LR;\n" ++
    "hidden [shape = plaintext, label = \"\"];\n" ++
    
    "node [shape = doublecircle];\n" ++
     (foldrGlue (\ f str -> 
                      gvshow f ++ " [label = \"\"];\n" ++ str)
        finals
     ("node [shape = point];\n" ++
     foldrGlue (\ (st,num) str ->
                    gvshow st ++ " [label=\"" ++ show num ++ "\"];\n" ++ str)
       (zip states [1..])
       (
          ("hidden -> " ++ gvshow start ++ ";\n" ++ 
          (foldrGlue (\ st str ->
                foldrGlue (\ c str ->
                           gvshow st ++ " -> " ++ gvshow (trans st c) ++ " [label = \"" ++ gvshow c ++ "\"];\n" ++ str)
                      inputs
                      str)
           states
           "}\n")))))

writeGraphViz :: (GvShow input, GvShow state) => String -> Dfa input state -> IO ()
writeGraphViz filename d =
  writeFile filename (toGraphViz d)