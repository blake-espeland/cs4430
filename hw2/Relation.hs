module Relation where

import Util

-- a relation over a type 'state' is just a list of ordered pairs of elements of that type
type Rel state = [(state,state)]

-- composition of relations
compose :: Eq state => Rel state -> Rel state -> Rel state
compose r1 r2 = [(x,z) | (x,y) <- r1, (y',z) <- r2, y == y']

-- identity relation
identity :: [state] -> Rel state
identity states = [(s,s) | s <- states]

-- intersection of relations
intersect :: Eq state => Rel state -> Rel state -> Rel state
intersect r1 r2 = [(x,y) | (x,y) <- r1, (x',y') <- r2, x == x', y == y']

-- union of relations
union :: Eq state => Rel state -> Rel state -> Rel state
union r1 r2 = r1 ++ r2

-- transitive closure
tClose :: Ord state => Rel state -> Rel state
tClose r = fixedPoint r helper
 where helper r' = canonOrd $ union r' (compose r r') 

rClose :: Eq state => [state] -> Rel state -> Rel state
rClose states r = union r (identity states)

-- reflexive-transitive closure
rtClose :: Ord state => [state] -> Rel state -> Rel state
rtClose states r = tClose (rClose states r)

symmClose :: Eq state => Rel state -> Rel state
symmClose r = union r [(s',s) | (s,s') <- r]

equivClose :: Ord state => [state] -> Rel state -> Rel state
equivClose states r = rtClose states (symmClose r)

-- return the image of a set of states under the relation
image :: Eq state => Rel state -> [state] -> [state]
image r ss = [s2 | s1 <- ss, (s1',s2) <- r , s1 == s1']

-- apply the relation, returning a set of outputs
applyRel :: Ord state => Rel state -> state -> [state]
applyRel r s = canonOrd [s'' | (s',s'') <- r , s' == s]

-- return the equivalence class of a state, assuming equiv is indeed an equivalence relation
ec :: Ord state => Rel state -> state -> [state]
ec = applyRel

