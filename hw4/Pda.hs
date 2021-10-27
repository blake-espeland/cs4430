module Pda where

import GvShow
import Data.List
import StackRelation
import Util

data Action stack = Push stack | Pop stack | Noop
  deriving (Eq,Ord)

instance Show stack => Show (Action stack) where
  show (Push x) = '+' : show x
  show (Pop x) = '-' : show x
  show Noop = "."

isPop :: Action stack -> Bool
isPop (Pop _) = True
isPop _ = False 

type Trans input state action = input -> Rel state action

data Pda input state stack = Pda {
  states :: [state], -- Q: all states.  
  inputs :: [input], -- Sigma: all possible inputs.
  stacksyms :: [stack], -- Gamma: all possible stack symbols.
  trans :: Trans input state (Action stack), -- R: transition relations
  eps :: Rel state (Action stack), -- E: epsilon transitions
  start :: state, -- start state
  final :: [state]} -- final states

{- we don't have a bottom of stack symbol explicitly, but the code below assumes there is one, that appears at most
   once in any stack.  It also assumes that all stack symbols except the bottom of stack symbol represent a request to
   read at least one input character -}

simplifyActions :: Eq stack => [Action stack] -> [Action stack]
simplifyActions [] = []
simplifyActions (Noop : s) = simplifyActions s
simplifyActions (Push x : s) =
 case simplifyActions s of
   (Pop x' : s) | x == x' -> s
   u -> Push x : u
simplifyActions (Pop x : s) = Pop x : simplifyActions s

multistep :: (Ord state, Ord stack) => Pda input state stack -> Trans [input] state [Action stack]
multistep n@(Pda states _ _ trans eps _ _) x = h x 
  where cutoff = length x + 2 -- adding 1 for bottom of stack symbol
        eps' = rtClose states cutoff simplifyActions (embedActions eps)
        h [] = eps'
        h (c:cs) = compose eps' $ compose (embedActions (trans c)) (multistep n cs)


legalActions :: Eq stack => [Action stack] -> Bool
legalActions s = all (not . isPop) $ simplifyActions s

accepts :: (Ord state, Ord stack) => Pda input state stack -> [input] -> Bool
accepts n@(Pda _ _ _ trans eps start final) str =
  any ok (multistep n str)
  where ok (s1,acts,s2) = s1 == start && elem s2 final && legalActions acts

toGraphViz :: (GvShow input, GvShow state, Eq state, Show stack) => Pda input state stack -> String
toGraphViz (Pda states inputs _ trans eps start finals) =
    "digraph pda {\n" ++
    "rankdir = LR;\n" ++
    "hidden [shape = plaintext, label = \"\"];\n" ++
    "node [shape = doublecircle];\n" ++
    (foldrGlue (\ f str -> 
                   gvshow f ++ " [label = \"\"];\n" ++ str)
               finals
       ("node [shape = point];\n" ++
        "hidden -> " ++ gvshow start ++ ";\n" ++ 
       -- loop over states st
       (foldrGlue (\ st str ->
                -- loop over input letters c
                foldrGlue (\ c str ->
                         -- loop over states with an edge labeled c from st
                         foldrGlue (\ (a,st') str ->
                                  gvshow st ++ " -> " ++ gvshow st' ++ " [label = \"" ++ gvshow c ++ ", " ++ show a ++ "\"];\n" ++ str)
                               [(a,s') | (s,a,s') <- (trans c), s == st] 
                               str)
                      inputs
                      str)
         states
         -- add epsilon transitions
         (foldrGlue (\ (st,a,st') str ->
                       gvshow st ++ " -> " ++ gvshow st' ++ " [label = \"" ++ show a ++ "\"];\n" ++ str)
           eps
           "}\n"))))

-- write the given NFA in GraphViz format to the file named filename.
writeGraphViz :: (GvShow input, GvShow state, Eq state, Show stack) => String -> Pda input state stack -> IO ()
writeGraphViz filename d =
  writeFile filename (toGraphViz d)
