--
-- Date:    9/23/2008
-- Contact: Berlin Brown <berlin dot brown at gmail.com>
-- Source License: See New BSD license at:
-- http://www.opensource.org/licenses/bsd-license.php
--
-- Copyright (C) 2008 Botnode.com (Berlin Brown). All Rights Reserved.
--
-- Additional Resources:
-- [1] http://haskell.org/ghc/docs/latest/html/libraries/GLUT/Graphics-UI-GLUT.html
-- [2] http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/rect.html
-- File: Canon.hs
--
-- Bugs:
-- [1] Cells aren't exactly rendered in between the grid lines.  I had trouble
--     sizing up the cells properly.  The cells and the grid lines may not
--     line up properly but it looks ok with what I have now.
module Main where

import Graphics.UI.GLUT
import System.Exit
import Data.IORef
import Monad
import Control.Concurrent (threadDelay)

import GameLife

type SVector = (Double, Double, Double)
type Point   = (Double, Double)

glpoint :: Double -> Double -> Vertex2 GLdouble
glpoint x y = (Vertex2 x y)

glcolor :: Double -> Double -> Double -> IO ()
glcolor x y z = color $ Color3 x y z

-- Invoke the function N number of times
ftimes :: Int -> (Int -> IO ()) -> IO ()
ftimes 0 func = return ()
ftimes n func = do
  func n
  ftimes (n-1) func

renderVertLine :: Double -> IO ()
renderVertLine x = do
  glcolor 0.0 1.0 0.0
  rect (glpoint x (-0.8)) (glpoint (x+0.004) 0.8)

renderHorizLine :: Double -> IO ()
renderHorizLine y = do
  glcolor 0.0 1.0 0.0
  rect (glpoint (-0.8) y) (glpoint 0.8 (y+0.004))

renderMarker :: SVector -> IO ()
renderMarker pos = do
  let (x, y, _) = pos
  glcolor 0.0 0.0 1.0
  rect (glpoint ((-0.007)+x) ((-0.04) +y)) (glpoint (0.007+x) (0.04 +y))
  rect (glpoint ((-0.04) +x) ((-0.007)+y)) (glpoint (0.04 +x) (0.007+y))

renderCell :: (Double, Double) -> IO ()
renderCell (i, j) = do
  let sz    = 0.055
      x     = (i * sz) - 0.8
      y     = (j * sz) - 0.8
  glcolor 1.0 0.0 0.0
  rect (glpoint x y) (glpoint (sz+x) (sz+y))

--
-- Core render function: update the game of life state and
-- then render the GL objects to screen on this frame.
renderCellWorld :: IORef (CellWorld) -> Window -> DisplayCallback
renderCellWorld simref win = do
  clear [ColorBuffer]
  sim <- readIORef simref
  -- Render the cell world grid lines
  ftimes 32 (\n -> (renderVertLine  (((fromIntegral n)*0.05)- 0.8)))
  ftimes 32 (\n -> (renderHorizLine (((fromIntegral n)*0.05)- 0.8)))
  -- Render the cell world
  renderBoard sim
  -- Update the simulation IO reference and move to the next
  -- cell generation            
  simref $= (nextGeneration sim)
  --simref $= sim
  threadDelay 10
  -- GL swap buffers
  swapBuffers

--
-- Purpose: Render the Cell World data structure
-- to the window.
renderBoard :: CellWorld -> IO ()
renderBoard world =   
    -- Extract each cell in the IxJ CellWorld and determine
    -- when to render that cell.
    mapM_ (\(rw,j) -> (mapM_ (\(st,i) ->
                                  do { if (st == alive) 
                                       then renderCell (i,j)
                                       else return ()
                                  })
                       (zip rw [0..]))) (zip world [0..])

quit :: Key -> KeyState -> Modifiers -> Position -> IO ()
quit (Char 'q') _ _ _ = exitWith ExitSuccess >> return ()
quit _ _ _ _          = return ()

--
-- Purpose: main entry point for the GL application
main = do
  putStrLn "Running cannon simulation"
  -- Init OpenGL
  getArgsAndInitialize  

  initialWindowSize $= Size 400 400
  initialDisplayMode $= [DoubleBuffered]
  world <- generateWorld
  sim <- newIORef world
  -- Create the GL window and run simulation
  w <- createWindow "Life Simulation"
  keyboardMouseCallback $= Just quit
  clearColor $= (Color4 0 0 0 0)
  loadIdentity
  -- Render callback function
  displayCallback $= renderCellWorld sim w
  idleCallback $= Just (postRedisplay (Just w))
  -- Init glCallback loop
  mainLoop             

-- End of File;

