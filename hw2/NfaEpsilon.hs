{- Just like Nfa except allowing epsilon-transitions -}
module NfaEpsilon where

import Data.List(nub,sort)
import Util
import GvShow
import Relation

{- the type for transition functions: given an input,
   return the relation corresponding to edges labeled with that
   input.
-}
type Trans input state = input -> Rel state

data Nfa input state =
  Nfa {
    states :: [state], -- Q: all states.  
    inputs :: [input], -- Sigma: all possible inputs.
    trans :: Trans input state, -- R: transition relations.
    eps   :: Rel state, -- E: epsilon transitions (as a relation)
    start :: [state], -- start states
    final :: [state]} -- final states
    
{- given a NFA, return the multistep transition function,
   which returns the list of states you reach when processing
   a sequence of inputs from a given starting state -}
multistep :: Ord state => Nfa input state -> Trans [input] state
multistep n@(Nfa states _ trans eps _ _) = h
  where eps' = rtClose states eps
        h [] = eps'
        h (c:cs) = compose eps' $ compose (trans c) (multistep n cs)

accepts :: Ord state => Nfa input state -> [input] -> Bool
accepts n@(Nfa _ _ trans eps start final) str =
  not ( null ( intersect (multistep n str) [(s,f) | s <- start, f <- final]))

----------------------------------------------------------------------
-- showing an NFA

foldrGlue f xs str = foldr f str xs

instance (Show input , Show state) => Show (Nfa input state) where
  show (Nfa states inputs trans eps start final) =
    "States: " ++ show states ++ "\n" ++
    "Alphabet: " ++ show inputs ++ "\n" ++
    "Transitions:\n" ++
       (foldrGlue (\ c str ->
                 " " ++ show c ++ ": " ++ show (trans c) ++ "\n" ++ str)
         inputs
         ("Epsilon transitions: " ++ show eps ++ "\n" ++
         "Start states: " ++ show start ++ "\n" ++
         "Final states: " ++ show final))

----------------------------------------------------------------------
-- to GraphViz format

-- this assumes distinct states and inputs are shown distinctly
toGraphViz :: (GvShow input, GvShow state, Eq state) => Nfa input state -> String
toGraphViz (Nfa states inputs trans eps start finals) =
    "digraph nfaEpsilon {\n" ++
    "rankdir = LR;\n" ++
    "hidden [shape = plaintext, label = \"\"];\n" ++
    "node [shape = doublecircle];\n" ++
    (foldrGlue (\ f str -> 
                   gvshow f ++ " [label = \"\"];\n" ++ str)
               finals
       ("node [shape = point];\n" ++
       (foldrGlue (\ (st,num) str ->
                    gvshow st ++ " [label=\"" ++ show num ++ "\"];\n" ++ str)
         (zip states [1..])
        (foldrGlue (\ start str ->
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
         -- add epsilon transitions
         (foldrGlue (\ (st,st') str ->
                       gvshow st ++ " -> " ++ gvshow st' ++ " [label = \"\"];\n" ++ str)
           eps
           "}\n"))))))

-- write the given NFA in GraphViz format to the file named filename.
writeGraphViz :: (GvShow input, GvShow state, Eq state) => String -> Nfa input state -> IO ()
writeGraphViz filename d =
  writeFile filename (toGraphViz d)

-- apply the given function f to the states throughout the automaton
mapNfa :: (state -> state') -> Nfa input state -> Nfa input state'
mapNfa f (Nfa states inputs trans eps start final) =
  let mapRel r = [(f x, f y) | (x,y) <- r] in
    Nfa (map f states) inputs
      (\ c -> mapRel (trans c))
      (mapRel eps)
      (map f start)
      (map f final)

