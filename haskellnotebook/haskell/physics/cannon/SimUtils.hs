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
module SimUtils where

import Data.Ix
import Debug.Trace

maxIter :: Double
maxIter = 100
simpi   = 3.1415926535897932 :: Double
pi_180  = simpi / (180.0 :: Double)

type SVector = (Double, Double, Double)
type Point   = (Double, Double)

data Simulation = Simulation {
      vm       :: !Double,    -- Magnitude of muzzle velocity, m/s
      alpha    :: !Double,    -- Angle from y-axis (upward) to the cannon.
			      -- When this angle is zero the cannon is pointing
			      -- straight up, when it is 90 degrees, the cannon
		              -- is horizontal
      gamma    :: !Double,    -- Angle from x-axis, in the x-z plane to the cannon.
                              -- When this angle is zero the cannon is pointing in
			      -- the positive x-direction, positive values of this angle
                              -- are toward the positive z-axis
      lencan   :: !Double,    -- This is the length of the cannon, m
      yb       :: !Double,    -- This is the base elevation of the cannon
      targpos  :: !SVector,   -- Pos of center of target
      targparm :: !SVector,   -- Len, Width, Height of target
      shpos    :: !SVector,   -- Shell position vector
      grav     :: !Double,    -- acceleration due to gravity
      time     :: !Double,
      tinc     :: !Double,
      shmin    :: !SVector,   -- Mininum X,Y,Z shell positions
      shmax    :: !SVector,   -- Max X,Y,Z shell positions
      iter     :: !Double     -- Current simulation iteration
    } deriving (Read)
             
instance Show Simulation where
    show sim = let (tx, ty, tz) = (targpos sim)
                   (i, j, k)    = (shpos sim)
                   (a, g)       = ((alpha sim), (gamma sim))
                   (i1, j1, k1) = (shmin sim)
                   (i2, j2, k2) = (shmax sim)
                   idx          = (iter sim)
                   t            = (time sim)
                   step_pos     = ( ((i2-i1) / idx ), ((j2-j1) / idx) ) :: Point
               in "\n ** Simulation Data ("  ++(show t)++"/"++(show idx)++"):"
                      ++"\nShell Position: <"++(show (shpos sim))++">"
                      ++"\nTarget Pos: <"    ++(show (targpos sim))++">"
                      ++"\nMin Shell: <"     ++(show (shmin sim))++">"
                      ++"\nMax Shell: <"     ++(show (shmax sim))++">"
                      ++"\nStep Avg: "       ++(show step_pos)

-- Initialize the simulation
-- g = 9.8 m/s2 = 32.2 ft/s2
initSimulation :: (Real v) => v -> v -> v -> v -> v -> Simulation
initSimulation muz_vel a g ln bs_elev = 
    Simulation {
  vm       = (realToFrac muz_vel),
  alpha    = (realToFrac a),
  gamma    = (realToFrac g),
  lencan   = (realToFrac ln),
  yb       = (realToFrac bs_elev),
  targpos  = ((realToFrac 400), (realToFrac 35), (realToFrac 45)),
  targparm = ((realToFrac 10),  (realToFrac 15), (realToFrac 20)),
  shpos    = ((realToFrac 0),   (realToFrac 0),  (realToFrac 0)),
  grav     = 9.8,
  time     = 0,
  tinc     = 0.05,
  iter     = 1.0,
  shmax    = ((realToFrac (-1)),  (realToFrac (-1)),  (realToFrac (-1))),
  shmin    = ((realToFrac 99999), (realToFrac 99999), (realToFrac 99999)) }
                  
--
-- Calculate direction cosines for cannon orientation.
simulationDirCos :: Simulation -> SVector
simulationDirCos sim =
    let (a,   g)  = ((alpha sim),(gamma sim))
        len       = lencan sim
        b         = len * cos((90.0 - a) * pi_180)
	lx        = b   * cos(g * pi_180)   -- x-component of barrel length
	ly        = len * cos(a * pi_180)   -- y-component of barrel length
	lz        = b   * sin(g * pi_180)   -- z-component of barrel length
        cospos = (lx/len, ly/len, lz/len) :: SVector
    in cospos

-- Crude method for updating min and max
-- Update the 6 values if the value has changed
updateMinMax :: Simulation -> Simulation
updateMinMax sim = 
    sim { shmin = (i1,j1,k1), shmax = (i2,j2,k2) }
    where (si,   sj,   sk)   = (shpos sim)
          (mini, minj, mink) = (shmin sim)
          (maxi, maxj, maxk) = (shmax sim)
          i1 | si <  mini    = si
             | otherwise     = mini
          i2 | si >= maxi    = si
             | otherwise     = maxi
          j1 | sj <  minj    = sj
             | otherwise     = minj
          j2 | sj >= maxj    = sj
             | otherwise     = maxj
          k1 | sk <  mink    = sk
             | otherwise     = mink
          k2 | sk >= maxk    = sk
             | otherwise     = maxk

simulationTick :: SVector -> Simulation -> Simulation
simulationTick (cosx, cosy, cosz) sim =
    let len    = lencan sim
        (a, g) = ((alpha sim),(gamma sim))
        t      = (time sim)
        elev   = (yb sim)
        v      = (vm sim)
        xe     = len * cos((90.0-a) * pi_180) * cos(g * pi_180)
	ze     = len * cos((90.0-a) * pi_180) * sin(g * pi_180)
	-- Calculate the position vector at this time
	i      = v * cosx * t + xe;
	j      = (elev + len * cos(a*pi_180))+(v*cosy*t)-(0.5*g*t*t)
	k      = v * cosz * t + ze;
    in updateMinMax(sim { shpos    = (i, j, k),
                          iter     = (iter sim) + 1,
                          time     = t + (tinc sim) })

simpleSimulation :: Simulation -> Int -> Simulation
simpleSimulation sim i
    | t  > maxIter = sim
    | sj <= 0      = sim
    | otherwise    = simpleSimulation newsim (i+1)
    where newsim             = (simulationTick (simulationDirCos sim) sim)
          t                  = (time  newsim)
          (si,sj,sk)         = (shpos newsim)
             
-- End of File;