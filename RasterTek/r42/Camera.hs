module Camera (Camera(..),initialize) where

import Graphics.GL

import Flow.Parameters
import Flow.Render
import Flow.Update
import Maths

data Camera = Camera {
    getPosition :: [GLfloat],
    getRotation :: [GLfloat],
    getView :: [GLfloat],
    getBaseView :: [GLfloat],
    getReflection :: [GLfloat] }
    deriving (Eq, Show)

initialize :: IO Camera
initialize = return . (\mx -> Camera [0,0,-10] [0,0,0] mx mx mx) . take 16 . cycle $ [1,0,0,0,0]

instance Render Camera where
    render camera = do
        let rMatrix = rotationPitchYawRollLH (getRotation camera)
            lookat = add (getPosition camera) $ multiply3 [0,0,1] rMatrix
            up = multiply3 [0,1,0] rMatrix

        return . (,) True $ camera {
            getView = lookAtLH (getPosition camera) lookat up }

instance Update Camera where
    update camera (F height) = do
        let rMatrix = rotationPitchYawRollLH (getRotation camera)
            position = [
                head . getPosition $ camera,
                2*height - (getPosition camera!!1),
                last . getPosition $ camera ]
            radians = (getRotation camera!!1)
            lookat = [
                sin radians + (head . getPosition) camera,
                position!!1,
                cos radians + (last . getPosition) camera ]
            up = multiply3 [0,1,0] rMatrix
        
        return . (,) True $ camera {
            getReflection = lookAtLH position lookat up }
            
rotationPitchYawRollLH :: [GLfloat] -> [[GLfloat]]
rotationPitchYawRollLH [pitch,yaw,roll] = let
    cpitch = cos pitch
    cyaw = cos yaw
    croll = cos roll
    spitch = sin pitch
    syaw = sin yaw
    sroll = sin roll
    in
    [ [croll*cyaw + spitch*sroll*syaw, sroll*cpitch, croll*(-syaw) + spitch*sroll*cyaw]
    , [(-sroll)*cyaw + spitch*croll*syaw, croll*cpitch, sroll*syaw + spitch*croll*cyaw]
    , [cpitch*syaw, -spitch, cpitch*cyaw] ]
