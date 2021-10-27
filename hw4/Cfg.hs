{- a simple version of CFGs, where nonterminals and terminals
   both have type String. -}
module Cfg (module Cfg, module ProdTree) where

import Data.List
import Util
import ProdTree

type Grammar =
  (String {- start symbol -},
   [Prod] {- productions -},
   (String -> Bool) {- returns True for Strings which are nonterminals -} )

grammarNt (_,_,nt) = nt

{- check that the start symbol and all left-hand sides of productions are indeed non-terminals -}
legalGrammar :: Grammar -> Bool
legalGrammar (s,ps,nt) = nt s && all (nt . fst) ps

{- check if a ProdTree is truly a legal derivation of a given list of symbols for a given Grammar.

   1. Check that the ProdTree's root is labeled with the start symbol.
   2. Check that the list of leaves of the ProdTree indeed is the given list of symbols
   3. Check that the productions used are contained in the grammar's productions -}
legalProdTree :: Grammar -> ProdTree -> [String] -> Bool
legalProdTree g@(s,ps,nt) t ss =
  (rootLabel t == s) &&
  (leaves nt t == ss) &&
  (canonOrd (computeProductions nt t) `isSubsequenceOf` canonOrd ps)