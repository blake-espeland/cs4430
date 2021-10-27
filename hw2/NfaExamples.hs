module NfaExamples where

import GvShow
import qualified Dfa -- to use names x from here you must write Dfa.x.  This is what 'qualified' means
import Dfa(Dfa(Dfa)) -- this says to import the names Dfa for the type and Dfa for its constructor, unqualified
import Nfa
import SubsetConstruction
import ExampleBasics

{- automaton from exercise 5.1 of Kozen -}
trans5'1 :: Trans AB ThreeStates
trans5'1 A = [(Q1,Q1),(Q2,Q3)]
trans5'1 B = [(Q1,Q1),(Q1,Q2),(Q2,Q3)]

ex5'1 :: Nfa AB ThreeStates
ex5'1 = Nfa [Q1 ..] [A,B] trans5'1 [Q1] [Q3]

run5'1 = multistep ex5'1 [A,B,A]

accepting5'1 = accepts ex5'1 [B,A,A,B,B]
rejecting5'1 = accepts ex5'1 [B,A,A,B,A,B]

det5'1 = determinize ex5'1

-----------------------------------------------
smallert :: Trans AB Bool
smallert A = [(True,True),(True,False)]
smallert B = [(True,True)]

smaller :: Nfa AB Bool
smaller = Nfa [True,False] [A,B] smallert [True] [False]