module Flow.Parameters (Parameters(..)) where

import Graphics.UI.GLFW

data Parameters = None
    | II Int Int
    | IF Int Float
    deriving (Eq,Show)
