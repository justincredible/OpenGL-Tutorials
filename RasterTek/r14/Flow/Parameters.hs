module Flow.Parameters (Parameters(..)) where

import Graphics.UI.GLFW

data Parameters = None
    | II Int Int
    deriving (Eq,Show)
