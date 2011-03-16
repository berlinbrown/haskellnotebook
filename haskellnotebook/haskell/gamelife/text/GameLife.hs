--
-- Author: Berlin Brown
-- File:   GameLife.hs
-- Date:   8/21/2008
--
-- Short Descr: Conway's Game of Life in Haskell
--
-- Rules from Wikipedia:
-- http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
-- =======================
-- All text is available under the terms of the GNU Free Documentation License. 
-- (See Copyrights for details.) 
-- =======================
-- 1. Any live cell with fewer than two live neighbours dies, 
--    as if by loneliness.
-- 2. Any live cell with more than three live neighbours dies, 
--    as if by overcrowding.
-- 3. Any live cell with two or three live neighbours lives, 
---   unchanged, to the next generation.
-- 4. Any dead cell with exactly three live neighbours comes to life.
--
-- Loosely based on the API description from:
-- http://www.cs.chalmers.se/Cs/Grundutb/Kurser/e4fun/labs/GameOfLife.html
--
module GameLife where

import IO
import System.Random
import Debug.Trace

maxIter   :: Int
maxIter   = 300
randThres = 0.45
boardSize = 30
alive     = True
dead      = False

type Board     = String 
type CellState = Bool
type Cell      = (Int,Int)
type CellWorld = [[CellState]]

-- Determine if cell pos is within bounds
boundsChk :: Cell -> Bool
boundsChk (x,y)
          | x < 0             = False
          | x > (boardSize-1) = False
          | y < 0             = False
          | y > (boardSize-1) = False
          | otherwise         = True

-- Get a list of cells surrounded around a this particular cell position.
allCells :: Cell -> [Cell]
allCells (x,y) = [(nx,ny) | nx <- [x+1,x-1,x], ny <- [y+1,y-1,y], 
                   ((x,y) /= (nx,ny) && boundsChk (nx,ny)) ]

-- Get the state of the cells around one cell
allCellsState :: CellWorld -> Cell -> [CellState]
allCellsState world loc = map (\pos -> (getState world pos)) (allCells loc)

-- Get Cell State at this location
getState :: CellWorld -> Cell -> CellState
getState world (x,y) = ((world !! y) !! x)

-- Use the game of life rules to determine of the cell state of this cell
lifeRules :: [CellState] -> CellState -> CellState
lifeRules states cur
          | (alive == cur) && (ct <  2)              = False
          | (alive == cur) && (ct >  3)              = False
          | (alive == cur) && (ct == 2) || (ct == 3) = True
          | (dead  == cur) && (ct == 3)              = True
          | otherwise                                = False
    where ct = (length (filter id states))

--
-- What is the state of the cell based on what is around
nextGenCell :: CellWorld -> Cell -> CellState
nextGenCell world loc =
    (lifeRules (allCellsState world loc) (getState world loc))

-- Given the state of the current world, recreate the environment
-- Call map twice, to iterate through all of the cells
nextGeneration :: CellWorld -> CellWorld
nextGeneration world = 
    map (\(rw,i) -> (map (\(state,j) -> 
                          (nextGenCell world (j,i))) (zip rw [0..])))
            (zip world [0..])

-- ===============================================
-- Board Utilities
-- ===============================================

-- Create the default board view.
defaultBoard :: Board
defaultBoard = replicate (boardSize * boardSize) '.'

-- Purpose: Pretty print the game of life board.
pprintBoard :: Board -> IO ()
pprintBoard board = do loop board
    where
      loop []   = return ()
      loop grid = do putStrLn (take boardSize grid)
                     loop     (drop boardSize grid)

getCellWorld :: CellWorld -> Board
getCellWorld world = concat newgen
    where newgen = map (\rw -> (map (\state ->
                                     (if (state == alive) then '#' else '.'))
                                rw)) world

pprintWorld :: CellWorld -> IO ()
pprintWorld world = pprintBoard (getCellWorld world)

-- Purpose: Generate a list of random values.
randomVals :: IO [Float]
randomVals =
    -- Create a potential list of 1 to 100 values
    -- consisting of values from 0 to 1
    do { n <- randomRIO (1,100)
       ; sequence (replicate n (randomRIO (0, 1)))
       }

generateState :: IO [CellState]
generateState =
    mapM (\x -> do r <- randomVals
                   let h = head r
                   if (h > randThres) then return alive else return dead) 
             [0..(boardSize-1)]

generateWorld :: IO CellWorld
generateWorld =
    mapM (\x -> generateState) [0..(boardSize-1)]

-- End of File