module RegexpTests where

--import ExampleBasics
import Regexp
import NfaEpsilon
import GvShow
import qualified Dfa
import SubsetConstructionEps
import DfaOperations

data ABC = A | B | C
  deriving (Show , Eq , Ord, Enum)

instance GvShow ABC where
  gvshow = show

e1 = Star (Concat (Char A) (Char B))

n1 = toNfa e1

-- A* + A*
e2 = Or (Star (Char A)) (Star (Char A))

n2 = toNfa e2

e3 = Or (Concat (Char A) (Star (Char B)))
        (Concat (Char C) (Star (Char B)))

n3 = toNfa e3