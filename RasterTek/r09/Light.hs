module Light (Light(..),initialize) where

import Graphics.GL

data Light = Light {
    getDirection :: [GLfloat],
    getDiffuse :: [GLfloat],
    getAmbient :: [GLfloat] }
    deriving (Eq, Show)

initialize :: IO Light
initialize = return $ Light [1,0,0] [1,1,1,1] [0.15,0.15,0.15,1]
