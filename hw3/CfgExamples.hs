module CfgExamples where

import Data.List
import Cfg
import qualified ProdTree as PT

balparens :: Grammar
balparens =
  ("X", [ ("X",[]),
          ("X",["(","X",")"]),
          ("X",["X","X"])], (== "X"))

promote :: String -> [String]
promote = map (\ s -> [s])

s1 :: [String]
s1 = promote "(()()())"

derivOpenClose :: ProdTree
derivOpenClose = Node "X" [ leaf "(" , Node "X" [] , leaf ")" ]

pt1 :: ProdTree
pt1 = Node "X"
        [ leaf "(" ,
          Node "X"
           [ derivOpenClose,
             Node "X"
               [ derivOpenClose,
                 derivOpenClose
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

d2 :: ProdTree
d2 = Node "L"
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

s3 :: [String]
s3 = promote "[8,8]"

d3 :: ProdTree
d3 = Node "L" [
      leaf "[", 
      Node "S" [
        Node "L" [
          leaf "8"], 
        Node "R" [
          leaf ",",
          Node "L" [
            leaf "8"],
          leaf "R"]], 
      leaf "]"]

s4 :: [String]
s4 = promote "[[8,8,8]]"

d4 :: ProdTree
d4 = Node "L" [
      leaf "[",
      Node "S" [
        Node "L" [
          leaf "[",
          Node "S" [
            Node "L" [
              leaf "8"],
            Node "R" [
              leaf ",",
              Node "L" [
                leaf "8"],
              Node "R" [
                leaf ",",
                Node "L" [
                  leaf "8"],
                leaf "R"]]],
            leaf "]"],
          leaf "R"],
        leaf "]"]