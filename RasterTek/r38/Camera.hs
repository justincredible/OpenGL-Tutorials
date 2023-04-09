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
    getReflection :: [GLfloat] }
    deriving (Eq, Show)

initialize :: IO Camera
initialize = return . (Camera [0,0,-1] [0,0,0] <*> id) . take 16 . cycle $ [1,0,0,0,0]

instance Render Camera where
    render camera@(Camera position rotation _ _) = do
        let rMatrix = rotationPitchYawRollLH rotation
            lookat = add position $ multiply3 [0,0,1] rMatrix
            up = multiply3 [0,1,0] rMatrix

        return . (,) True $ camera {
            getView = buildViewMatrix position lookat up }

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
            getReflection = buildViewMatrix position lookat up }
            
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

buildViewMatrix position lookat up = let
    zAxis@[zax,zay,zaz] = (normalize . minus lookat) position
    xAxis@[xax,xay,xaz] = (normalize . cross up) zAxis
    yAxis@[yax,yay,yaz] = cross zAxis xAxis
    in
    [ xax, yax, zax, 0
    , xay, yay, zay, 0
    , xaz, yaz, zaz, 0
    , negate . dot xAxis $ position, negate . dot yAxis $ position, negate . dot zAxis $ position, 1 ]
