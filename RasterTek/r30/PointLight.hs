module PointLight (PointLight(..),initialize) where

import Graphics.GL

data PointLight = PointLight {
    getPosition :: [GLfloat],
    getAmbient :: [GLfloat],
    getDiffuse :: [GLfloat],
    getSpecular :: [GLfloat],
    getPower :: GLfloat }
    deriving (Eq, Show)

initialize :: [GLfloat] -> [GLfloat] -> IO PointLight
initialize position colour = return $ PointLight position [0.15,0.15,0.15,1] colour [1,1,1,1] 16
