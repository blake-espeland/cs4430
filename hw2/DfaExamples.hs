module DfaExamples where

import Dfa
import qualified DfaOperations as Ops
import Relation
import ExampleBasics
import Util

---------------------------------------------------------
-- Example DFA from class 8/23

trans8'23 :: Trans AB ThreeStates
trans8'23 Q1 A = Q1
trans8'23 Q1 B = Q2
trans8'23 Q2 A = Q3
trans8'23 Q2 B = Q2
trans8'23 Q3 A = Q3
trans8'23 Q3 B = Q3

class8'23 :: Dfa AB ThreeStates
class8'23 = Dfa [Q1,Q2,Q3] [A,B] trans8'23 Q1 [Q2]

s8'23 = multistep class8'23 Q1 [A,A,A,B,B]
accepting8'23 = accepts class8'23 [A,A,A,B,B]
rejecting8'23 = accepts class8'23 [A,A]

r8'23 = run class8'23 [A,A,A,B,B]

--------------------------------------------------
{- accepting strings of the form [B,A,B,A,...] -}
baTrans :: Trans AB FourStates
baTrans S0 B = S1
baTrans S1 A = S2
baTrans S2 B = S1
baTrans _ _ = S3 -- rejecting state

ba :: Dfa AB FourStates
ba = Dfa [S0 ..] [A,B] baTrans S0 [S2]

sba = multistep ba S0 [B,A,B]
acceptingba = accepts ba [B,A,B,A,B,A]
rejectingba = accepts ba [B,A,B,A,A]

rba = run ba [B,A,B,A,A]

---------------------------------------------------------
-- DFA from exercise 3.2 of Kozen.  It accepts
-- exactly the strings containing three consecutive a's

transEx3'2 :: Trans AB FourStates
transEx3'2 S0 A = S1
transEx3'2 S0 B = S0
transEx3'2 S1 A = S2
transEx3'2 S1 B = S0
transEx3'2 S2 A = S3
transEx3'2 S2 B = S0
transEx3'2 S3 A = S3
transEx3'2 S3 B = S3

ex3'2 :: Dfa AB FourStates
ex3'2 = Dfa [S0 ..] [A,B] transEx3'2 S0 [S3]

s3'2 = multistep ex3'2 S0 [B,A,A,A,B]
accepting3'2 = accepts ex3'2 [B,A,A,A,B]
rejecting3'2 = accepts ex3'2 [B,A,A,B,A,B]



----------------------------------------------------------------------
-- closure operations

star :: AB -> Dfa AB Bool 
star x = Dfa [True,False] [A,B] trans True [True]
  where trans True y | x == y = True
        trans _ _ = False

unionEx :: Dfa AB (Bool, Bool)
unionEx = Ops.union (star A) (star B)

-- non-minimal DFA to recognize {A}
justa :: Dfa AB FourStates
justa = Dfa [S0,S1,S2,S3] [A] delta S0 [S1]
 where delta S0 A = S1
       delta S1 A = S2
       delta S2 A = S3
       delta S3 A = S3

equiv1 :: Rel FourStates
equiv1 = equivClose [S0 .. S3] [(S2,S3)]

justaQ :: Dfa AB [FourStates]
justaQ = Ops.quotient justa equiv1

tr1 :: Trans AB FourStates
tr1 S0 A = S1
tr1 S0 B = S2
tr1 S1 _ = S2
tr1 S2 _ = S1
tr1 S3 _ = S0