{
module Grammar where
import Data.Char
import ProdTree
import System.Environment
}

%name balparens
%tokentype { Char }
%error { parseError }

%token
    '{'  { '{' }
    '}'  { '}' }
    '('  { '(' }
    ')'  { ')' }

%%

Q : { Node "X" [] }
  | S { $1 }

S : S P { Node "X" [$1, $2] }
  | P   { $1 }

P : '(' Q ')' { Node "X" [leaf "(", $2 , leaf ")"] }
  | '{' Q '}' { Node "X" [leaf "{", $2 , leaf "}"] }

{

parseError :: String -> a
parseError tks = error ("Parse error: " ++ show tks)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [f] -> do
      s <- readFile f
      writeGraphViz "output.gv" (== "X") $ balparens $ filter (not . isSpace) s
    _ ->
     putStrLn "Run with the name of a file to parse.\n"
}

