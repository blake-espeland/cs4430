module Debug where

import TM
import Util
import UniversalTM


data TestWrapper input state tape = TestWrapper {
    tm :: TM input state tape,
    inputs :: [input],
    utm :: TM input state tape
}

-- Adding Left End Marker to beginning of encoding
addLEM :: String -> String
addLEM e = "!" ++ e

-- stepTM :: TM input state tape -> state -> input -> [state, input]


debug :: (UEncode input, UEncode state, UEncode tape) =>
    TM input state tape -> [input] -> IO ()
debug tm i = putStrLn (inputU tm i)