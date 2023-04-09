module Camera (Camera,initialize,getView) where

import Graphics.GL

import Flow.Render
import Maths

data Camera = Camera {
    getPosition :: [GLfloat],
    getRotation :: [GLfloat],
    getView :: [GLfloat] }
    deriving (Eq, Show)

initialize :: IO Camera
initialize = return . Camera [0,0,-10] [0,0,0] . take 16 . cycle $ [1,0,0,0,0]

instance Render Camera where
    render camera@(Camera position rotation _) = do
        let rMatrix = rotationPitchYawRollLH rotation
            lookat = add position $ multiply3 [0,0,1] rMatrix
            up = multiply3 [0,1,0] rMatrix

        return . (,) True $ camera { getView = buildViewMatrix position lookat up }

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
