module ShadowLight (ShadowLight,initialize,getPosition,getLookAt,getAmbient,getDiffuse,getDirection,viewMatrix,projectionMatrix,orthographicMatrix) where

import Graphics.GL

import Flow.Parameters
import Flow.Update
import Maths

data ShadowLight = ShadowLight {
    getPosition :: [GLfloat],
    getLookAt :: [GLfloat],
    getAmbient :: [GLfloat],
    getDiffuse :: [GLfloat],
    getDirection :: [GLfloat],
    getView :: [GLfloat],
    getProjection :: [GLfloat],
    getOrthographic :: [GLfloat] }
    deriving (Eq, Show)

initialize :: IO ShadowLight
initialize = return
    . (\matrix -> ShadowLight [0,0,-1] [0,0,0] [0.15,0.15,0.15,1] [1,1,1,1] [0,0,1] matrix matrix matrix)
    $ identityLH

instance Update ShadowLight where
    update light None = return (True, light {
        getView = lookAtLH (getPosition light) (getLookAt light) [0,1,0] })

    update light (FF near far) = return (True, light {
        getProjection = perspectiveFovLH (pi/2) 1 near far })

    update light (F4 width height near far) = return (True, light {
        getOrthographic = orthographicLH width height near far })

    update light _ = do
        putStrLn "Incorrect shadow light parameters."
        return (False,light)

viewMatrix = getView

projectionMatrix = getProjection

orthographicMatrix = getOrthographic
