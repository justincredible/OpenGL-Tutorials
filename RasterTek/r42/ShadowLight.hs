module ShadowLight (ShadowLight,initialize,getPosition,getLookAt,getAmbient,getDiffuse,viewMatrix,projectionMatrix) where

import Graphics.GL

import Flow.Parameters
import Flow.Update
import Maths

data ShadowLight = ShadowLight {
    getPosition :: [GLfloat],
    getLookAt :: [GLfloat],
    getAmbient :: [GLfloat],
    getDiffuse :: [GLfloat],
    getView :: [GLfloat],
    getProjection :: [GLfloat] }
    deriving (Eq, Show)

initialize :: IO ShadowLight
initialize = return . (ShadowLight [0,0,-1] [0,0,0] [0.15,0.15,0.15,1] [1,1,1,1] <*> id) . take 16 . cycle $ [1,0,0,0,0]

instance Update ShadowLight where
    update light None = return (True, light {
        getView = lookAtLH (getPosition light) (getLookAt light) [0,1,0] })

    update light (FF near far) = return (True, light {
        getProjection = perspectiveFovLH (pi/2) 1 near far })

    update light _ = do
        putStrLn "Incorrect shadow light parameters."
        return (False,light)

viewMatrix = getView

projectionMatrix = getProjection
