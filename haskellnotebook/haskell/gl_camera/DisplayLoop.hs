----------------------------------------
{-  Module      : DisplayLoop
    Copyright   : BSD License
    License     : 
    Maintainer  : Berlin Brown
    Stability   : 
    Portability : 
    Description : Display
    TODO        : 
    Date        : 9/20/2007
    File        : DisplayLoop.hs
-}
----------------------------------------

module DisplayLoop where

import System.Exit ( exitWith, ExitCode(..) )

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import IO
import Debug.Trace ( trace, putTraceMsg )

import MechGenericObjects

--
-- Keyboard Handler
mechKeyPressed :: KeyboardMouseCallback
mechKeyPressed (Char '\27') Down _ _ = exitWith ExitSuccess
mechKeyPressed _            _    _ _ = return ()

--
-- Define a Mech Vector Type
type MVector = (Double, Double, Double)

--
-- Camera Data Type with position and angle
-- Vectors.  Note, the use of strict annotation
data Camera = Camera {
      cameraPos         :: !MVector,
      cameraLookAt      :: !MVector,
      cameraAngle       :: !MVector,
      cameraRot         :: !MVector,
      cameraCenterAxis  :: !MVector,
      cameraYaw         :: !MVector,
      cameraPitch       :: !MVector,
      cameraRoll        :: !MVector
    } deriving (Show, Read)

data SimpleMech = SimpleMech {
      mechPos         :: !MVector,
      mechRot         :: !MVector
    } deriving (Read)

instance Show SimpleMech where
    show mech = let (xrot, yrot, zrot) = (mechRot mech)
        in "roty <" ++ (show yrot) ++ ">"

--
-- Convert from a tuple of double to an OpenGL vector
toGLVertex :: MVector -> Vertex3 GLdouble
toGLVertex (a,b,c) =  Vertex3 a b c

-- Convert from a tuple of double to an OpenGL vector
toGLVector :: MVector -> Vector3 GLdouble
toGLVector (a,b,c) =  Vector3 a b c

--
-- Using gluLookAt, set the camera lookAt parameters
-- including position and look at.
--
-- gluLookAt(CAMERA->position[0], CAMERA->position[1], CAMERA->position[2], 
--  x, h, y, 0.0f, 1.0f, 0.0f);
-- eyeX, eyeY, eyeZ = Specifies the position of the eye point.
-- centerX, centerY, centerZ = Specifies the position of the reference point.
--
setCameraLookAt :: Camera -> IO ()
setCameraLookAt camera = do
  let curCameraPos = (cameraPos camera)
      curCameraLook = (cameraLookAt camera)
      upVector = (0, 1, 0)
  lookAt (toGLVertex curCameraPos)
             (toGLVertex curCameraLook) 
             (toGLVector upVector)

--
-- Given a angle delta value and a Mech object, 
-- rotate the mech on the Y-axis.
rotateMech :: (Double, SimpleMech) -> SimpleMech
rotateMech (angle, mech) =
  -- Calculate a new rotation
  let (xrot,yrot,zrot) = (mechRot mech)
      newyrot = newyrot + angle
  in SimpleMech { mechRot = (xrot, newyrot, zrot),
                  mechPos = (mechPos mech) }

--
-- Initialize a camera
-- Only set the camera position, camera angle and rotation
-- (Position, Camera Angle, Camera Rotation)
initCamera :: (Real posv) => (posv, posv, posv)-> (posv, posv, posv) -> (posv, posv, posv) -> Camera
initCamera (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) = 
    Camera { 
  cameraPos   = ((realToFrac x1), (realToFrac y1), (realToFrac z1)),
  cameraAngle = ((realToFrac x2), (realToFrac y2), (realToFrac z2)),
  cameraLookAt= ((realToFrac x3), (realToFrac y3), (realToFrac z3)),
  cameraRot        = (0, 0, 0),
  cameraCenterAxis = (0, 0, 0),
  cameraYaw        = (0, 0, 0),
  cameraPitch      = (0, 0, 0),
  cameraRoll       = (0, 0, 0) }

initMech :: (Real posv) => (posv, posv, posv)-> (posv, posv, posv) -> SimpleMech
initMech (x1, y1, z1) (x2, y2, z2) = 
    SimpleMech { 
  mechPos = ((realToFrac x1), (realToFrac y1), (realToFrac z1)),
  mechRot = ((realToFrac x2), (realToFrac y2), (realToFrac z2)) }

debugMech :: SimpleMech -> IO ()
debugMech mech = do
    let (xrot, yrot, zrot) = (mechRot mech)
    putTraceMsg $ "mech=" ++ (show mech)

--
-- Core display game loop, including rendering the scene
-- and swapping the GL double buffer.
--
-- Note: write mech ref is not used
displayGameLoop :: IORef Camera -> IORef SimpleMech -> IORef GLdouble -> IO ()
displayGameLoop cameraRef mechRef angleRotRef = do
  camera <- readIORef cameraRef
  mech <- get mechRef
  angleRot <- get angleRotRef

  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  setCameraLookAt camera

  let (xrot, yrot, zrot) = (mechRot mech)
  -- Render Game Objects (preserving = push/pop)
  preservingMatrix $ do
    -- C routine = glRotatef(angle, 0.0f, 1.0f, 0.0f);
    rotate angleRot (Vector3 0 1 (0 :: GLdouble))
    mechCube 0.6

  preservingMatrix $ do
    translate (Vector3 0 (-4.2) (0 :: GLdouble))
    renderPlain 1.4

  -- Set new mech position
  angleRotRef $= angleRot + 0.1
  mechRef $= (rotateMech (0.1, mech))
  
  -- OpenGL, swap buffers
  swapBuffers
  flush

--
-- Idle function, do nothing
idle = do
  postRedisplay Nothing
