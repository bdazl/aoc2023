module Parser 
( parse
, digit
, alpha
, choiceL
, munchRest
, munchNums1
, munchAlpha1
, countNums
)
where

import Data.Char
import Text.ParserCombinators.ReadP

parse :: ReadP a -> String -> Maybe a
parse p s = if null plist 
            then Nothing
            else extractFinal plist where
    plist = readP_to_S p s

extractFinal :: [(a, String)] -> Maybe a
extractFinal xs = if snd l == ""
                  then Just (fst l)
                  else Nothing where
    l = last xs

-- Local, exclusive left-biased choice
choiceL :: [ReadP a] -> ReadP a
choiceL []     = pfail
choiceL [p]    = p
choiceL (p:ps) = p <++ choiceL ps

munchRest :: ReadP String
munchRest = munch (\_ -> True)

munchNums1 :: ReadP Int
munchNums1 = do
    ns <- munch1 isDigit
    return (read ns)

munchAlpha1 :: ReadP String
munchAlpha1 = do
    xs <- munch1 isAlpha
    return xs

countNums :: Int -> ReadP Int
countNums c = do
    parse <- count c digit
    return (read parse)

digit :: ReadP Char
digit = satisfy isDigit

alpha :: ReadP Char
alpha = satisfy isAlpha
