module ProdTree(module ProdTree,module Data.Tree) where

import Data.Tree

leaf :: a -> Tree a
leaf x = Node x []

data TreePath = TreePath [Integer]

type ProdTree = Tree String

type Prod = (String,[String]) {- (left hand side, right hand side) -}

instance Show TreePath where
  show (TreePath xs) = "l_" ++ foldr (\ a str -> show a ++ "_" ++ str) "e" xs

{- p is the current path in the tree, which is used to name the graphviz nodes.
   kstr is the string to print after we are all done printing p -}
renderEdges :: (String -> Bool) -> TreePath -> ProdTree -> String -> String
renderEdges nt p@(TreePath cs) (Node x ns) kstr = 
  show p ++ "[label=\"" ++ x ++ "\"];\n" ++
  case ns of
    [] | nt x -> let p' = TreePath (1:cs) in
      show p' ++ "[label=\"\"];\n" ++
      show p ++ " -> " ++ show p' ++ ";\n" ++ kstr
    _ ->      
     foldr (\ (n,c) str -> let p' = TreePath (c:cs) in 
                            renderEdges nt p' n (show p ++ " -> " ++ show p' ++ ";\n" ++ str))
      kstr
      (zip ns [1..])

{- nt tells whether or not a symbol is a nonterminal -}
writeGraphViz :: String -> (String -> Bool) -> ProdTree -> IO ()
writeGraphViz filename nt ts =
  writeFile filename $
  "digraph nfa {\n" ++
  "rankdir = TB;\n" ++
  "node [shape = plaintext];\n" ++
  renderEdges nt (TreePath []) ts "}"

getLabels :: [ProdTree] -> [String]
getLabels = map rootLabel

{- we compute the set of productions that must have been used to create some
   derivation -}
computeProductions :: (String -> Bool) -> ProdTree -> [ Prod ]
computeProductions nt (Node s ts) =
  (if nt s then [(s,getLabels ts)] else []) ++ (concat $ map (computeProductions nt) ts)

{- compute the leaves of a tree in left-to-right order -}
leaves :: (String -> Bool) -> ProdTree -> [String]
leaves nt (Node a []) | nt a = []         -- the leaf is actually the empty string
                      | otherwise = [a]
leaves nt (Node a ts) = concat $ map (leaves nt) ts
