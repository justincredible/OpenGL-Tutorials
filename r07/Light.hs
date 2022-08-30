module Light (Light(..),initialize) where

import Graphics.GL

data Light = Light {
    getDirection :: [GLfloat],
    getDiffuse :: [GLfloat] }
    deriving (Eq, Show)

initialize :: IO Light
initialize = return $ Light [0,0,1] [1,1,1,1]
