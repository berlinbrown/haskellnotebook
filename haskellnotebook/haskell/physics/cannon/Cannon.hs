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
-- Based on cannon.c from
-- Most comments attributed to David Bourg.
-- ======================
-- PHYSICS FOR GAME DEVELOPERS	
-- CHAPTER 2 EXAMPLE PROGRAM
-- PURPOSE: To demonstrate 3D particle kinematics
-- BY: David Bourg
-- Physics Logic and Comments - Copyright 2000 by David Bourg
-- ======================
module Main where

import Graphics.UI.GLUT
import System.Exit
import Data.IORef
import Monad
import SimUtils
import Control.Concurrent (threadDelay)
import System.Posix.Unistd

glpoint :: Double -> Double -> Vertex2 GLdouble
glpoint x y = (Vertex2 x y)

glcolor :: Double -> Double -> Double -> IO ()
glcolor x y z = color $ Color3 x y z

renderAxis :: IO ()
renderAxis = do
  glcolor 0.0 1.0 0.0
  rect (glpoint (-0.007) (-0.04))  (glpoint 0.007 0.04)
  rect (glpoint (-0.04)  (-0.007)) (glpoint 0.04  0.007)

renderMarker :: SVector -> IO ()
renderMarker pos = do
  let (x, y, _) = pos
  glcolor 0.0 0.0 1.0
  rect (glpoint ((-0.007)+x) ((-0.04) +y)) (glpoint (0.007+x) (0.04 +y))
  rect (glpoint ((-0.04) +x) ((-0.007)+y)) (glpoint (0.04 +x) (0.007+y))

renderSimulation :: IORef (Simulation) -> Window -> DisplayCallback
renderSimulation simref win = do
  clear [ColorBuffer]
  sim <- readIORef simref
  -- Use round to convert to the double value to integral
  let newsim       = simulationTick (simulationDirCos sim) sim
      flag         = (odd (round (iter sim)))
      (xb, yb, zb) = (shpos sim)
      t            = (time sim)
      (x,  y)      = (xb * 0.0001, yb * 0.0001)

  -- Render a simple block
  renderAxis
  -- Render frames per second marker
  when flag (renderMarker ((-0.9::Double), (-0.9::Double), (0::Double)))
  -- render the moving marker
  renderMarker (x-0.9, y-0.9, (0::Double))
                            
  --putStrLn (show sim)
  putStrLn $ "t:" ++(show t)++" # gl-x: "++(show x)++" y:"++(show y)
  -- Update the simulation IO reference
  simref $= newsim
  --usleep 200000
  --threadDelay 10
  -- GL swap buffers
  swapBuffers

quit :: Key -> KeyState -> Modifiers -> Position -> IO ()
quit (Char 'q') _ _ _ = exitWith ExitSuccess >> return ()
quit _ _ _ _          = return ()

--
-- Purpose: main entry point for the GL application
main = do
  putStrLn "Running cannon simulation"
  -- Init OpenGL
  getArgsAndInitialize  
  -- Run the simulation as a dry run to size the screen elements
  -- properly
  let initsim = (initSimulation 10.0 25.0 0.0 12.0 10.0)
      dryrun  = simpleSimulation initsim 0
  putStrLn (show dryrun)
  initialWindowSize $= Size 400 400
  initialDisplayMode $= [DoubleBuffered]
  sim <- newIORef initsim
  -- Create the GL window and run simulation
  w <- createWindow "Simulation"
  keyboardMouseCallback $= Just quit
  clearColor $= (Color4 0 0 0 0)
  loadIdentity
  -- Render callback function
  displayCallback $= renderSimulation sim w
  idleCallback $= Just (postRedisplay (Just w))
  -- Init glCallback loop
  mainLoop             

-- End of File;

