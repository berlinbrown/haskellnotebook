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

import SimUtils

runSimulation :: Simulation -> Int -> IO Int
runSimulation sim i
    | t  > maxIter = do { putStrLn "Exiting - 3, max reached"    ; return 3 }
    | sj <= 0      = do { putStrLn "Exiting - 2, reached ground" ; return 2 }
    | otherwise    = do { putStrLn $ "Tick ("++(show i)++"):\n"++(show sim) 
                       ; (runSimulation newsim (i+1)) }
    where newsim             = (simulationTick (simulationDirCos sim) sim)
          t                  = (time  newsim)
          (si,sj,sk)         = (shpos newsim)

main = do
  putStrLn "Running cannon simulation"
  runSimulation (initSimulation 50.0 25.0 0.0 12.0 10.0) 0
             
-- End of File;