import TM
import UniversalTM
import Util
import GvShow
import Debug
import TMExamples(tripletm)



sigma = "01.,#"
sigNoPD = "01.,#"
stackChars = sigma ++ "abcdxrh! "
stackCharsNoH = sigma ++ "abcdxr! "
stackCharsNoR = sigma ++ "abcdxh! "
stackCharsNoPD = sigNoPD ++ "abcdxrh! "

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-
Resetting read artifacts
Goes from start state a to end state b
Requres 2 intermediary states
-}
resetArtifacts :: Integer -> Integer -> [Trans Integer Char]
resetArtifacts a b = 
    goLeft a 'a' '0' (a + 1) ++
    goLeft a 'b' '1' (a + 1) ++
    goLeft a 'c' '1' (a + 1) ++
    goLeft a 'd' '0' (a + 1) ++
    goLeft a 'h' ',' (a + 1) ++
    goLeft a 'r' '.' (a + 1) ++
    goLeft (a + 1) 'a' '0' a ++
    goLeft (a + 1) 'b' '1' a ++
    goLeft (a + 1) 'c' '1' a ++
    goLeft (a + 1) 'd' '0' a ++
    goLeft (a + 1) 'h' ',' a ++
    goLeft (a + 1) 'r' '.' a ++
    loopLeft a ".,#x" ++
    loopLeft (a + 1) ".,#x" ++
    goRight a '!' '!' (a + 2) ++
    goRight (a + 1) '!' '!' (a + 2) ++
    goToEnd (a + 2) b
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- SEARCHING FOR NEXT STATE
-------------------------------------------------------------------------------
{-
Rule for writing bits:
If first char read is 0:
    Other char is 0 -> write a
    Other char is 1 -> write b
If first char read is 1:
    Other char is 0 -> write d
    Other char is 1 -> write c
-}
-- | Start before direction end at state b
-- | 3 states
markReadPt :: Integer -> Integer -> [Trans Integer Char]
markReadPt a b =
    loopRight a "ac10." ++
    goLeft a ',' ',' (a + 1) ++
    goRight (a + 1) '.' 'r' b
-- | 1 states
checkForZero :: Integer -> [Trans Integer Char]
checkForZero a =
    loopRight a "ac" ++
    goRight a '0' 'a' (a + 1) ++
    goRight a '1' 'b' (a + 1)
-- | 1 states
checkForOne :: Integer -> [Trans Integer Char]
checkForOne a =
    loopRight a "ac" ++
    goRight a '0' 'd' (a + 1) ++
    goRight a '1' 'c' (a + 1)
-- | Start from end, finish before direction
-- | 8 states
readZeroFromTrans :: Integer -> [Trans Integer Char]
readZeroFromTrans a =
    fromStatetoTrans a ++ -- a -> a + 3

    goToEnd (a + 4) a ++ -- Getting next character from state
    goToEnd (a + 7) a ++

    goRight (a + 3) 'b' 'b' (a + 5) ++
    goRight (a + 3) 'd' 'd' (a + 5) ++
    nextTransition (a + 5) (a + 3) ++ -- goto next transition
    goRight (a + 6) 'b' 'b' (a + 8) ++
    goRight (a + 6) 'd' 'd' (a + 8) ++
    nextTransition (a + 8) (a + 3) ++

    checkForZero (a + 3) ++
    goRight (a + 3) '.' '.' (a + 6) ++ -- done reading state
    checkForZero (a + 6) ++
    goRight (a + 6) '.' '.' (a + 9)
-- | Start from end, finish before direction
-- | 8 states
readOneFromTrans :: Integer -> [Trans Integer Char]
readOneFromTrans a =
    fromStatetoTrans a ++ -- a -> a + 3

    goToEnd (a + 4) a ++ -- Getting next character from state
    goToEnd (a + 7) a ++

    goRight (a + 3) 'b' 'b' (a + 5) ++
    goRight (a + 3) 'd' 'd' (a + 5) ++
    nextTransition (a + 5) (a + 3) ++ -- goto next transition
    goRight (a + 6) 'b' 'b' (a + 8) ++
    goRight (a + 6) 'd' 'd' (a + 8) ++
    nextTransition (a + 8) (a + 3) ++

    checkForOne (a + 3) ++ -- a + 3 -> a + 4
    goRight (a + 3) '.' '.' (a + 6) ++ -- done reading state
    checkForOne (a + 6) ++ -- a + 6 -> a + 7
    goRight (a + 6) '.' '.' (a + 9)
{- 
If there's an incorrect bit, go to next transition
cur -> curent state
par -> parent state to return to
-}
nextTransition :: Integer -> Integer -> [Trans Integer Char]
nextTransition cur par =
    loopRight cur "01.abcd" ++
    goRight cur ',' ',' par
-- | Start at end, go to beginning of transitions
-- | 3 states
fromStatetoTrans :: Integer -> [Trans Integer Char]
fromStatetoTrans a =
    loopLeft a "01,.abcdhr" ++
    goLeft a '#' '#' (a + 1) ++
    loopLeft (a + 1) "01,.abcdhr" ++
    goLeft (a + 1) '#' '#' (a + 2) ++
    loopLeft (a + 2) "01,.abcdhr" ++
    goRight (a + 2) '#' '#' (a + 3)
-- | Starting from the end
readForNextTrans :: Integer -> [Trans Integer Char]
readForNextTrans a = 
    loopLeft a "ac" ++
    goRight a '0' 'a' (a + 1) ++
    readZeroFromTrans (a + 1) ++
    markReadPt (a + 10) (a + 23) ++ -- Exit here
    goRight a '1' 'c' (a + 12) ++
    readOneFromTrans (a + 12) ++ -- a + 12 -> a + 20
    markReadPt (a + 21) (a + 23) -- Exit here
-------------------------------------------------------------------------------
-- SEARCHING FOR NEXT STATE
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- WRITING NEW STATE
-------------------------------------------------------------------------------
-- | Starting from the end of read transition
-- | 6 States
-- | Ends at end of state
writeNewState :: Integer -> [Trans Integer Char]
writeNewState a =
    loopLeft a "ac" ++
    goRight a '.' '.' (a + 7) ++
    loopLeft (a + 7) "ac" ++
    -- Exiting and moving r
    goRight (a + 7) '.' 'r'  (a + 8) ++
    loopRight (a + 8) "ac.01" ++
    goRight (a + 8) 'r' '.' (a + 9) ++
    goToEnd (a + 9) (a + 10) ++ -- Exit here

    goRight a '0' 'a' (a + 1) ++
    goRight (a + 7) '0' 'a' (a + 1) ++
    writeZeroToState (a + 1) ++
    goLeft (a + 3) 'r' 'r' a ++
    
    goRight a '1' 'c' (a + 4) ++
    goRight (a + 7) '0' 'a' (a + 1) ++
    writeOneToState (a + 4) ++
    goLeft (a + 6) 'r' 'r' a
-- | Starting from the char after char to be written
-- | 2 states
writeOneToState :: Integer -> [Trans Integer Char]
writeOneToState a = 
    goToEnd a (a + 1) ++
    loopLeft (a + 1) "10." ++
    goLeft (a + 1) 'a' '1' (a + 2) ++
    goLeft (a + 1) 'c' '1' (a + 2) ++
    loopLeft (a + 2) "10.,abcd#"
-- | Starting from the char after char to be written
-- | 2 states
writeZeroToState :: Integer -> [Trans Integer Char]
writeZeroToState a = 
    goToEnd a (a + 1) ++
    loopLeft (a + 1) "10.," ++
    goLeft (a + 1) 'a' '0' (a + 2) ++
    goLeft (a + 1) 'c' '0' (a + 2) ++
    loopLeft (a + 2) "10.,abcd#"
-------------------------------------------------------------------------------
-- WRITING NEW STATE
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- WRITING NEW CHAR TO TAPE
-------------------------------------------------------------------------------
-- Starting at end of state
writeCharToTape :: Integer -> [Trans Integer Char]
writeCharToTape a = 
    loopLeft a "ac" ++
    -- Write 0
    goLeft a '0' 'a' (a + 1) ++
    loopLeft (a + 1) stackCharsNoH ++
    goLeft (a + 1) 'h' 'h' (a + 2) ++
    loopLeft (a + 2) "01" ++
    goRight (a + 2) 'a' '0' (a + 3) ++
    goRight (a + 2) 'c' '0' (a + 3) ++
    goToEnd (a + 3) a ++
    -- Write 1
    goLeft a '1' 'c' (a + 4) ++
    loopLeft (a + 4) stackCharsNoH ++
    goLeft (a + 4) 'h' 'h' (a + 5) ++
    loopLeft (a + 5) "01" ++
    goRight (a + 5) 'a' '0' (a + 6) ++
    goRight (a + 5) 'c' '0' (a + 6) ++
    goToEnd (a + 6) a ++
    -- Exit condition
    goLeft a '.' '.' (a + 7)
-------------------------------------------------------------------------------
-- WRITING NEW CHAR TO TAPE
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- CHECKING IF FINAL STATE
-------------------------------------------------------------------------------
checkIfInFinalState :: Integer -> [Trans Integer Char]
checkIfInFinalState a =
    goBackToCurState a (a + 2) ++
    loopLeft (a + 2) "ac" ++
    goLeft (a + 2) '#' '#' (a + 34) ++  -- Accept here
    -- Read 0
    goLeft (a + 2) '0' 'a' (a + 3) ++
    goToFinalState (a + 3) (a + 7) ++
    loopLeft (a + 7) "ac" ++
    goLeft (a + 7) '0' 'a' (a + 8) ++
    goLeft (a + 7) '0' 'b' (a + 9) ++
    goLeft (a + 7) 'b' 'b' (a + 9) ++
    nextFinalState (a + 9) a (a + 33) ++ -- Exit here
    -- Read 1
    goLeft a '1' 'c' (a + 18) ++
    goToFinalState (a + 18) (a + 22) ++
    loopLeft (a + 22) "ac" ++
    goLeft (a + 22) '1' 'c' (a + 23) ++
    goLeft (a + 22) '1' 'd' (a + 24) ++
    goLeft (a + 22) 'd' 'd' (a + 24) ++
    nextFinalState (a + 24) a (a + 33) -- Exit here
goToFinalState :: Integer -> Integer -> [Trans Integer Char]
goToFinalState a b =
    loopLeft a stackCharsNoPD ++
    goLeft a '#' '#' (a + 1) ++
    loopLeft (a + 1) stackCharsNoPD ++
    goLeft (a + 1) '#' '#' (a + 2) ++
    loopLeft (a + 2) stackCharsNoPD ++
    goLeft (a + 2) '#' '#' (a + 3) ++
    goLeft (a + 3) ',' ',' b
goBackToCurState :: Integer -> Integer -> [Trans Integer Char]
goBackToCurState a b = 
    goToEnd a (a + 1) ++
    loopLeft (a + 1) "ac01" ++
    goLeft (a + 1) '.' '.' b
nextFinalState :: Integer -> Integer -> Integer -> [Trans Integer Char]
nextFinalState a b e = 
    resetState a (a + 8) ++
    loopLeft (a + 8) "abcd01" ++
    goLeft (a + 8) ',' ',' b ++
    goLeft (a + 8) '#' '#' e
resetState :: Integer -> Integer -> [Trans Integer Char]
resetState a b = 
    goBackToCurState a (a + 2) ++
    loopLeft (a + 2) "01" ++
    goLeft (a + 2) 'a' '0' (a + 3) ++
    goLeft (a + 2) 'c' '1' (a + 3) ++
    goLeft (a + 2) '#' '#' (a + 4) ++
    loopLeft (a + 3) "01" ++
    goLeft (a + 3) 'a' '0' (a + 2) ++
    goLeft (a + 3) 'c' '1' (a + 2) ++
    goLeft (a + 3) '#' '#' (a + 4) ++
    goToFinalState (a + 4) (a + 8)
-------------------------------------------------------------------------------
-- CHECKING IF FINAL STATE
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- MOVING IN GIVEN DIRECTION
-------------------------------------------------------------------------------
-- | Starts before final state encoding
moveInDirection :: Integer -> [Trans Integer Char]
moveInDirection a = 
    loopRight a stackCharsNoR ++
    goLeft a 'r' 'r' (a + 1) ++
    loopLeft (a + 1) "abcd10" ++ -- over g'
    goLeft (a + 1) '.' '.' (a + 2) ++
    loopLeft (a + 2) "abcd10" ++ -- over st'
    goLeft (a + 2) '.' '.' (a + 3) ++
    -- 0 -> Move Right
    goRight (a + 3) '0' '0' (a + 4) ++
    loopRight (a + 4) stackCharsNoH ++
    goRight (a + 4) 'h' ',' (a + 5) ++
    loopRight (a + 5) "01ac" ++
    goLeft (a + 5) ',' 'h' (a + 6) ++ -- Exit here
    -- 1 -> Move Left
    goRight (a + 3) '0' '0' (a + 7) ++
    loopRight (a + 7) stackCharsNoH ++
    goLeft (a + 7) 'h' ',' (a + 8) ++
    loopLeft (a + 8) "01ac" ++
    goLeft (a + 8) ',' 'h' (a + 6)   -- Exit here


-------------------------------------------------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------------------------------------------------
-- | Loop left at state a until reaching end, then go to state b
goToBeginning :: Integer -> Integer -> [Trans Integer Char]
goToBeginning a b = 
    loopLeft a stackChars ++
    goRight a '!' '!' b
-- | Loop right at state a until reaching end, then go to state b
goToEnd :: Integer -> Integer -> [Trans Integer Char]
goToEnd a b = 
    loopRight a stackChars ++
    goLeft a ' ' ' ' b
-------------------------------------------------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- MAKING START STATE.CHAR
-------------------------------------------------------------------------------
-- | 6 states
writeStartState :: Integer -> [Trans Integer Char]
writeStartState a = 
    loopRight a "01," ++
    goRight a '#' '#' (a + 1) ++
    loopRight a "01," ++
    goRight a '#' '#' (a + 2) ++
    loopRight (a + 2) "x" ++
    goRight (a + 2) '#' '#' (a + 6) ++ -- Exit here

    goRight (a + 2) '0' 'x' (a + 3) ++
    loopRight (a + 3) stackChars ++
    goLeft (a + 3) ' ' '0' (a + 5) ++

    goRight (a + 2) '1' 'x' (a + 4) ++
    loopRight (a + 4) stackChars ++
    goLeft (a + 3) ' ' '1' (a + 5) ++
    goToBeginning (a + 5) a
-- | 7 states
writeFirstChar :: Integer -> [Trans Integer Char]
writeFirstChar a =
    goToEnd a (a + 1) ++
    -- Going back to char
    loopLeft (a + 1) "01.," ++
    goLeft (a + 1) '#' '#' (a + 2) ++
    loopLeft (a + 2) "01.,ac" ++
    goRight (a + 2) '#' '#' (a + 3) ++
    loopRight (a + 3) "01" ++
    goRight (a + 3) '.' '.' (a + 4) ++
    loopRight (a + 4) "ac" ++
    goRight (a + 4) '.' 'h' (a + 7) ++
    goToEnd (a + 7) (a + 8) ++  -- Exit here
    -- Mark that char is read
    goRight (a + 4) '0' 'a' (a + 5) ++
    goRight (a + 4) '1' 'c' (a + 6) ++
    -- Go back to beginning
    loopRight (a + 5) stackChars ++
    goRight (a + 5) ' ' '0' (a + 1) ++
    loopRight (a + 6) stackChars ++
    goRight (a + 6) ' ' '1' (a + 1)
-------------------------------------------------------------------------------
-- MAKING START STATE.CHAR
-------------------------------------------------------------------------------


{-
TM/string combo is encoded as
e{Left endmarker}#
e{blank symbol}#
e{start state}#
e{final state(s)}#
e{transitions}#
e{string input}
-}

{-
. -> separates item within element
, -> separates elements within section
# -> separates sections
-}

msUniverse =
    TM [1 .. 90] sigma stackChars id ' ' '!' trans 1 [87]
    where 
        trans = 
            -- Finish at end of start state
            -- Exits to state a + 6
            writeStartState 1 ++

            -- Finish at end of read transition
            -- Exits to state a + 8
            writeFirstChar 7 ++

            ---------------- Main Loop -------------------

            -- Finish at end of read transition
            -- Exits to state a + 23
            readForNextTrans 15 ++ 
            -- "...01r,acb..."
            --       ^

            -- Finish at end of state
            -- Exits to state a + 10
            writeNewState 38 ++
            -- "...01 "
            --      ^

            -- Finish at end of state's state
            -- Exits to state a + 7
            writeCharToTape 48 ++
            -- "#...01.00...1 "
            --        ^
            
            -- Accepts or finishes at beginning of final states
            -- Exits to state a + 33
            -- Accepts to a + 34
            checkIfInFinalState 55 ++

            -- Finish at readhead char
            -- Exits to state a + 6
            moveInDirection 88

            -- Finish at 
            -- Exits to state 
            -- writeNewChar a

            -- Finish at end of string
            -- Exits to state 15
            -- resetArtifacts a b