module Day2 where

import Parser
import Data.Char
import Data.Maybe
import Control.Applicative
import Text.ParserCombinators.ReadP

type Game = (Int, [Bag])
type Bag = [Elem]
type Elem = (Int, String)

gameP :: ReadP Game
gameP = do
    string "Game"
    skipSpaces
    id <- munchNums1
    sepP ':'
    bags <- manyBagP
    return (id, bags)

manyBagP :: ReadP [Bag]
manyBagP = sepBy1 bagP (sepP ';')

bagP :: ReadP Bag
bagP = sepBy1 elemP (sepP ',')

sepP :: Char -> ReadP Char
sepP c = do
    satisfy (== c)
    skipSpaces
    return c

elemP :: ReadP Elem
elemP = do
    c <- munchNums1
    skipSpaces
    name <- munchAlpha1
    return (c, name)

assumeParse :: String -> Game
assumeParse = fromJust . (parse gameP)

parseAll :: [String] -> [Game]
parseAll = map assumeParse

isElemPossible :: Elem -> Bool
isElemPossible (n, "red") = n <= 12
isElemPossible (n, "green") = n <= 13
isElemPossible (n, "blue") = n <= 14
isElemPossible _ = True

isBagPossible :: Bag -> Bool
isBagPossible = (all (==True)) . (map isElemPossible)

isGamePossible :: Game -> Bool
isGamePossible (id, bags) = all (==True) (map isBagPossible bags)

solve :: [String] -> Int
solve xs = sum ps where
    gs = parseAll xs
    ps = [id | (id, bags) <- gs, isGamePossible (id, bags)]

solve' :: [String] -> Int
solve' _ = -1

main :: IO ()
main = do
    file <- readFile "input/day2.txt"
    let l = lines file
    print $ solve l
    print $ solve' l


ex1 :: [String]
ex1 = ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
       "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
       "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
       "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
       "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]

e1 = ex1 !! 0
b1 = "3 blue, 4 red"
bt1 = "3 blue, 4 red;"
