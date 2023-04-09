module Viewpoint (Viewpoint,initialize,viewMatrix,projectionMatrix) where

import Flow.Parameters
import Flow.Update
import Maths

data Viewpoint = Viewpoint {
    getPosition :: [Float],
    getLookAt :: [Float],
    getView :: [Float],
    getFieldOfView :: Float,
    getAspectRatio :: Float,
    getNearPlane :: Float,
    getFarPlane :: Float,
    getProjection :: [Float] }
    deriving (Eq, Show)

initialize :: IO Viewpoint
initialize = return $ Viewpoint [0,0,-1] [0,0,0] identityLH (pi/2) 1 0.1 1000 identityLH

instance Update Viewpoint where
    update viewpoint (LF2 position lookat) = return (True, viewpoint {
        getPosition = position,
        getLookAt = lookat,
        getView = lookAtLH position lookat [0,1,0] })
    
    update viewpoint (F4 fov aspect near far) = return (True, viewpoint {
        getFieldOfView = fov,
        getAspectRatio = aspect,
        getNearPlane = near,
        getFarPlane = far,
        getProjection = perspectiveFovLH fov aspect near far })

    update viewpoint _ = do
        putStrLn "Incorrect viewpoint parameters."
        return (False,viewpoint)

viewMatrix = getView

projectionMatrix = getProjection
