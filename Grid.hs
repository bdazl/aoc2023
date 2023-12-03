module Grid where

import Data.Maybe
import Common

-- This file is shamelessly copied from my prior AoC entries.
-- It is a pure, extremely inefficient, implementation of a 2D-array

-- Main 2D grid type
type Grid a = [[a]]
type Coord = (Int, Int)
type Size = (Int, Int)

-- apply function to all values in grid (that could become a new type of grid)
gridApply :: (Grid a -> Coord -> a -> b) -> Grid a -> Grid b
gridApply f mp = map rowop $ enumerate mp where
    rowop (y,row) = map (op y) $ enumerate row
    op y (x,v) = f mp (x,y) v

-- dimensions of Grid, assuming each row is of the same length
dimensions :: Grid a -> Size
dimensions xss = (length (head xss), length xss)

-- Remove coordinates not inside a grid of size
filterInside :: Size -> [Coord] -> [Coord]
filterInside (w,h) xs = filter cnd xs where
    cnd (x,y) = within x w && within y h
    within v bound = 0 <= v && v < bound

-- cut a list down to an index and its neighbour(s)
-- rowCut 2 [1, 2, 3, 4] = [2, 3, 4]
rowCut :: Int -> [a] -> [a]
rowCut n xs = take 3 . drop (n-1) $ xs

-- given a coord and a grid, try cutting the grid to a square
-- of neighbours surrounding the coord (including the input pt itself)
-- this works for edges as well, but then the dimensions differ.
-- the output is a sub-grid as well as a coord in the output grid that
-- corresponds to the input coord of the original map
nSquare :: Grid a -> Coord -> (Grid a, Coord)
nSquare mp (x,y) = (sqr, (nx,ny)) where
    sqr = map (rowCut x) . take (2+ny) . drop (y-1) $ mp
    ny = if y == 0 then 0 else 1
    nx = if x == 0 then 0 else 1

-- getAt value for coordinate of grid
getAt :: Grid a -> Coord -> Maybe a
getAt mp (x,y) = if insqr (x,y)
               then Just ((mp !! y) !! x)
               else Nothing where
    (dimx, dimy) = (length (head mp), length mp)
    insqr (a,b) = xinsqr a && yinsqr b
    xinsqr x_ = x_ >= 0 && x_ < dimx
    yinsqr y_ = y_ >= 0 && y_ < dimy

-- pick a pt on the grid and getAt its neighbours
-- the neighbouring pts are determined by the input coordinate generator
neighbours :: (Coord -> [Coord]) -> Coord -> Grid a -> [a]
neighbours cgen (x,y) mp = catMaybes . map (getAt sqr) . cgen $ (sx, sy) where
    (sqr, (sx,sy)) = nSquare mp (x,y)

-- neighbouring coordinate pickers
vertN :: Coord -> [Coord]
vertN (x,y) = [(x, y-1), (x, y+1)]

horizN :: Coord -> [Coord]
horizN (x,y) = [(x-1, y), (x+1, y)]

diagTLtoBRN :: Coord ->[Coord]
diagTLtoBRN (x,y) = [(x-1, y-1), (x+1, y+1)]

diagTRtoBLN :: Coord ->[Coord]
diagTRtoBLN (x,y) = [(x+1, y-1), (x-1, y+1)]

rimPlusN = combiner [vertN, horizN]
diagRimN = combiner [diagTLtoBRN, diagTRtoBLN]
rimN = combiner [rimPlusN, diagRimN]

-- combining coordinate pickers
combiner :: [a -> [b]] -> a -> [b]
combiner xs v = foldr (++) [] . map (apply v) $ xs where
    apply a f = f a
