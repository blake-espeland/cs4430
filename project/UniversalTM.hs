module UniversalTM where

import TM
import Util ( foldrGlue )
import Data.Char ( ord, chr )

lenCharInt = 8

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
                  "" -> []
                  s' -> w : splitOn p s''
                        where (w, s'') = break p s'

utape :: String
utape = ['0','1','#',',','.']

class UEncode a where
  lemToChar :: a -> Char
  uenc :: a -> Int -> String -- Wrapper that pads and reverses
  bitenc :: a -> String -- Puts into binary

fixTape :: String -> Int -> String
fixTape x l = concatMap (addComma . padZeros l) (splitOn (==',') x)

bitChar :: Bool -> Char
bitChar True = '1'
bitChar False = '0'

reverseEnc :: String -> String
reverseEnc = foldl (flip (:)) []

padZeros :: Int -> String -> String -- Had to make chars of equal length
padZeros l x
  | length x >= l || x == "#" = x
  | otherwise = padZeros l ('0' : x)

addComma :: String -> String
addComma x
  | x == "#" = x
  | otherwise = x ++ ","

instance UEncode Int where
  lemToChar = chr
  uenc x l = padZeros l . reverseEnc $ bitenc x
  bitenc x
    | x <= 0 = []
    | otherwise = bitChar ( x `mod` 2 == 1 ) : bitenc (x `div` 2)

-- Get true 8-bit binary representation
instance UEncode Char where
  lemToChar x = x
  uenc x l = padZeros l . reverseEnc $ bitenc x
  bitenc = bitenc . ord

instance UEncode Direction where
  uenc GoLeft l = bitenc GoLeft
  uenc GoRight l = bitenc GoRight
  bitenc GoLeft = "1"
  bitenc GoRight = "0"
  lemToChar GoLeft = '1'
  lemToChar GoRight = '0'

instance UEncode Integer where
  lemToChar = chr . fromIntegral
  uenc x l = padZeros l . reverseEnc $ bitenc x
  bitenc x = bitenc (fromIntegral x :: Int)

instance (UEncode state, UEncode tape) => UEncode (Trans state tape) where
  uenc x l = bitenc x
  bitenc (Trans st g d st' g') =
    dot st lenCharInt $
    dot g lenCharInt $
    dot d 1 $
    dot st' lenCharInt $
    dot g' lenCharInt ""

{- convenience function to encode a to a string of 1s and 0s,
   and then append another string.  The Char u separates the
   two strings.-}

bitencSep :: UEncode a => Char -> a -> Int -> String -> String
bitencSep u x l rest = uenc x l ++ (u : rest)

pound :: UEncode a => a -> Int -> String -> String
pound = bitencSep '#'
comma :: UEncode a => a -> Int -> String -> String
comma = bitencSep ','
dot :: UEncode a => a -> Int -> String -> String
dot = bitencSep '.'

list :: UEncode a => [a] -> Int -> String -> String
list xs l rest = foldrGlue f xs ('#' : rest)
  where f x = comma x l

encodeh :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape ->
          String {- string to follow this one -} ->
          String -- over Utape
encodeh (TM states inputs tapesyms _ blank leftend trans start final) rest =
  pound leftend lenCharInt $
  pound blank  lenCharInt $
  pound start lenCharInt $
  list final lenCharInt $
  -- need to add left end marker
  list trans 0 (fixTape (encLEM leftend ++ rest) lenCharInt)

encode :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape ->
          String -- over Utape
encode tm = encodeh tm ""

encLEM :: (UEncode tape) => tape -> String
encLEM le = uenc (lemToChar le) lenCharInt ++ ","

-- turn a TM and an input string into a single input for the universal TM
inputU :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape -> [input] -> String -- over Utape
inputU tm xs = encodeh tm (list xs 0 "")