module StackRelation where

import Util

-- a relation over a type 'state' is just a list of ordered pairs of elements of that type
type Rel state action = [(state,action,state)]

-- composition of relations
compose :: Eq state => Rel state [action] -> Rel state [action] -> Rel state [action]
compose r1 r2 = [(x,a ++ a',z) | (x,a,y) <- r1, (y',a',z) <- r2, y == y']

-- identity relation
identity :: [state] -> Rel state [action]
identity states = [(s,[],s) | s <- states]

-- union of relations
union :: Eq state => Rel state action -> Rel state action -> Rel state action
union r1 r2 = r1 ++ r2

simplifyRel :: (action -> action) -> Rel state action -> Rel state action
simplifyRel simplify r = [(s,simplify a,s') | (s,a,s') <- r]

applyCutoff :: Int -> Rel state [action] -> Rel state [action]
applyCutoff cutoff r = [(s,a,s') | (s,a,s') <- r , length a < cutoff]

-- transitive closure
tClose :: (Ord state, Ord action) => Int -> ([action] -> [action]) -> Rel state [action] -> Rel state [action]
tClose cutoff simplify r = fixedPoint r helper
 where helper r' = applyCutoff cutoff $ simplifyRel simplify $ canonOrd $ union r' (compose r r') 

rClose :: Eq state => [state] -> Rel state [action] -> Rel state [action]
rClose states r = union r (identity states)

rtClose :: (Ord state, Ord action) => [state] -> Int -> ([action] -> [action]) -> Rel state [action] -> Rel state [action]
rtClose states cutoff simplify r = tClose cutoff simplify (rClose states r)

mapActions :: (action -> action') -> Rel state action -> Rel state action'
mapActions f r = [(x,f a,y) | (x,a,y) <- r]

embedActions :: Rel state action -> Rel state [action]
embedActions = mapActions (\ x -> [x])