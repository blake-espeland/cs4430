module Transform where
import Cfg
import Data.List
import Util ( promote )
import CfgPumping
import ProdTree

{- Question 1:
    L -> [ S ]
    L -> [ ]
    L -> 8
    S -> L R
    R ->
    R -> , L R
-}
lst1Nt s = s `elem` ["L","S","R"]

lst1 :: Grammar
lst1 =
  ("L", [ ("L",["[","S","]"]),
          ("L",["[","]"]),
          ("L",["8"]),
          ("S",["L","R"]),
          ("S",["L"]),
          ("R",[",","L"]),
          ("R",[",","L","R"])], lst1Nt)

lst2Nt s = s `elem` ["L","S","R", "C", "LB", "RB"]

lst2 :: Grammar
lst2 =
  ("L", [ ("L",["LB","S","RB"]),
          ("L",["LB","RB"]),
          ("L",["8"]),
          ("S",["L","R"]),
          ("S",["L"]),
          ("R",["C","L"]),
          ("R",["C","L","R"]),
          ("C", [","]),
          ("LB", ["["]),
          ("RB", ["]"])], lst2Nt)

lst3Nt s = s `elem` ["L","S","R", "C", "LB", "RB", "N1", "N2"]

lst3 :: Grammar
lst3 =
  ("L", [ ("L",["LB","N1"]),
          ("L",["LB","RB"]),
          ("L",["8"]),
          ("N1",["S", "RB"]),
          ("S",["L","R"]),
          ("S",["L"]),
          ("R",["C","L"]),
          ("R",["C","N2"]),
          ("N2",["L", "R"]),
          ("C", [","]),
          ("LB", ["["]),
          ("RB", ["]"])], lst3Nt)


simple :: [String]
simple = promote "[8,8]"

pt :: ProdTree
pt = Node "L" [
        Node "LB" [
            leaf "["],
        Node "N1" [
          Node "S" [
              Node "L" [
                  leaf "8" ],
              Node "R" [
                  Node "C" [
                      leaf ","],
                  Node "L" [
                      leaf "8"]
              ]
          ],
          Node "RB" [
            leaf "]"]
        ]
     ]

test3 = legalProdTree lst3 pt simple

pumpedup :: Maybe ProdTree
pumpedup = pump lst3Nt pt 2