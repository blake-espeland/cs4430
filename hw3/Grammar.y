{
module GrammarS4 where
import Data.Char
import Data.List
import ProdTree
import System.Environment
}

%name inequality
%tokentype { Char }
%error { parseError }

%token
    '<'  { '<' }
    '+'  { '+' }
    '*'  { '*' }
    '('  { '(' }
    ')'  { ')' }
    'x'  { 'x' }
    'y'  { 'y' }

%%

L : PA '<' PA { Node "L" [$1, leaf "<", $3] }

PA : '(' PA ')' { Node "E" [leaf "(", $2, leaf ")"] }
   | PL { $1 }

T : T '*' A {Node "E" [$1, leaf "*", $3]}
  | A { $1 }

PL : T '+' PL { Node "E" [$1, leaf "+", $3] }
   | T { $1 }

A : 'x' { Node "E" [leaf "x"] }
  | 'y' { Node "E" [leaf "y"] }

{

parseError :: String -> a
parseError tks = error ("Parse error: " ++ show tks)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [f] -> do
      s <- readFile f
      writeGraphViz "outputS4.gv" (`elem` ["E", "L"]) $ inequality $ filter (not . isSpace) s
    _ ->
     putStrLn "Run with the name of a file to parse.\n"
}

