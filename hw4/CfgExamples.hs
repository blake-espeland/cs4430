module CfgExamples where

import Data.List
import Cfg
import qualified ProdTree as PT
import Util

balparens :: Grammar
balparens =
  ("X", [ ("X",[]),
          ("X",["(","X",")"]),
          ("X",["X","X"])], (== "X"))

s1 :: [String]
s1 = promote "(()()())"

openClose :: ProdTree
openClose = Node "X" [ leaf "(" , Node "X" [] , leaf ")" ]

pt1 :: ProdTree
pt1 = Node "X"
        [ leaf "(" ,
          Node "X"
           [ openClose,
             Node "X"
               [ openClose,
                 openClose
               ]],
          leaf ")" ]

lstNt s = elem s ["L","S","R"]

lst :: Grammar
lst =
  ("L", [ ("L",["[","S","]"]),
          ("L",["[","]"]),
          ("L",["8"]),
          ("S",["L","R"]),
          ("R",[]),
          ("R",[",","L","R"])], lstNt)

s2 :: [String]
s2 = promote "[8,[8,8]]"

pt2 :: ProdTree
pt2 = Node "L"
      [leaf "[",
       Node "S"
        [ Node "L"
            [ leaf "8" ],
          Node "R"
            [ leaf ",",
              Node "L"
                [ leaf "[",
                  Node "S"
                    [ Node "L"
                       [leaf "8"],
                      Node "R"
                       [ leaf ",",
                         Node "L"
                           [ leaf "8" ] ,
                         leaf "R" ]],
                  leaf "]"],
              leaf "R"] ],
       leaf "]"]

nt3 = (\ x -> elem x ["P","Q","S"])
balparens2 :: Grammar
balparens2 = ("Q", [("Q",[]),("Q",["S"]),
                     ("S",["S","P"]),("S",["P"]),
                     ("P",["(","Q",")"])],
                nt3)


s3 = promote "(()())"

openClose2 = Node "P"
               [ leaf "(",
                 leaf "Q",
                 leaf ")" ]
pt3 = Node "Q"
        [ Node "S"
            [ Node "P"
                [ leaf "(",
                  Node "Q"
                    [ Node "S"
                       [ Node "S"
                          [ openClose2 ] ,
                         openClose2 ] ],
                  leaf ")"]]]

test3 = legalProdTree balparens2 pt3 s3