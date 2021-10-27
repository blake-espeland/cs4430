{- Implements an algorithm for trying to take a production
   tree with a path that contains some nonterminal X twice,
   and pump in the tree rooted at the higher occurrence of X
   any number of times in place of the tree rooted at the
   lower occurrence of X.  See Lecture 22 of Kozen.
-}
module CfgPumping where

import Data.List
import Cfg
import ProdTree
import Util(Path,subtreeAt,contextTo,iter)

type Context = ProdTree -> ProdTree

{- Find two paths, the first a prefix of the second, to nodes
   with the same nonterminal; or else return Nothing
-}
findNestedNth :: (String -> Bool) {- is nonterminal -} ->
                  Path -> {- from the root of the starting tree -}
                  ProdTree -> {- current subtree -}
                  [(String,Path)] -> {- nonterminals we have seen so far, and where -}
                  Maybe (Path {- higher in tree -} , Path {- lower -})
findNestedNth nt path (Node x ts) seen =
  case find ((== x) . fst) seen of
    Nothing -> if nt x then
                 descend 0 ts
               else
                 Nothing -- stop at terminals
    Just (_,path') -> Just (path',path)
  where seen' = if null path then seen else (x,path) : seen {- don't add the root, so the context
                                                               we find is nontrivial -}
        descend _ [] = Nothing
        descend i (t : ts) =
          case findNestedNth nt (i : path) t seen' of
            Just p -> Just p
            Nothing -> descend (i+1) ts

{- Find two paths p1 and p2, where p1 leads to a node labeled
   with nonterminal X, and starting from that node, p2 leads
   to a node also labeled with X; or else return Nothing
-}
findNestedNt nt pt =
 case findNestedNth nt [] pt [] of
    Just (p1,p2) -> Just (reverse p1 , drop (length p1) (reverse p2))
    Nothing -> Nothing

{- decompose the ProdTree into Top, Middle, and Bottom. -}
splitProdTree :: (String -> Bool) {- is nonterminal -} ->
                  ProdTree -> 
                  Maybe (Context,Context,ProdTree)
splitProdTree nt pt =
  case findNestedNt nt pt of
    Nothing -> Nothing
    Just(p1,p2) ->
      let top = contextTo p1 pt
          middle = contextTo p2 (subtreeAt p1 pt)
          bottom = subtreeAt (p1 ++ p2) pt in
       Just (top,middle,bottom)

instantiate :: Context -> String -> ProdTree
instantiate c x = c (Node x [])

splitProdTree1 nt pt =
  do
    (top,middle,bottom) <- splitProdTree nt pt 
    Just (instantiate top "*" , instantiate middle "*", bottom)

writeTripleIf nt x =
  case x of
    Nothing -> return()
    Just (top,middle,bottom) ->
      do
        ProdTree.writeGraphViz "top.gv" nt top
        ProdTree.writeGraphViz "middle.gv" nt middle
        ProdTree.writeGraphViz "bottom.gv" nt bottom        

writeIf nt x =
  case x of
    Nothing -> return()
    Just t1 ->
      do
        ProdTree.writeGraphViz "output.gv" nt t1

pump nt pt i =
  case splitProdTree nt pt of
    Nothing -> Nothing
    Just (top,middle,bottom) -> Just $ top $ iter i middle bottom
  