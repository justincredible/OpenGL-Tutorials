module Light (Light(..),initialize) where

import Graphics.GL

data Light = Light {
    getDirection :: [GLfloat],
    getAmbient :: [GLfloat],
    getDiffuse :: [GLfloat],
    getSpecular :: [GLfloat],
    getPower :: GLfloat }
    deriving (Eq, Show)

initialize :: IO Light
initialize = return $ Light [1,0,0] [0.15,0.15,0.15,1] [1,1,1,1] [1,1,1,1] 32
