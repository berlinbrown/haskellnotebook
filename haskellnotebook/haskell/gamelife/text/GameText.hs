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
module Main (main) where

import IO
import System.Random
import Debug.Trace
import GameLife

-- Game Loop
gameLoop :: CellWorld -> Int -> IO ()
gameLoop init idx 
    | idx > maxIter = return ()
    | otherwise     = do putStrLn $ "** Generation: " ++ (show idx) ++ " **"
                         putStrLn $ (replicate 24 '=')
                         pprintWorld init
                         gameLoop (nextGeneration init) (idx+1)

-- Print the startup header message.
printHeader :: IO ()
printHeader = do
  putStrLn "Running Problem Set"
  putStrLn $ "" ++ (replicate 20 '=')
  putStrLn $ "# GameLife"
  putStrLn $ "" ++ (replicate 20 '=')

-- 
-- Purpose: main entry point of the program                   
main = do
  world <- generateWorld
  pprintWorld world
  gameLoop world 0
  putStrLn "INFO: Done (!)"
  
-- End of File