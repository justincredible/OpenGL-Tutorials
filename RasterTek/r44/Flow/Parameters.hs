module Flow.Parameters (Parameters(..)) where

import Graphics.UI.GLFW

data Parameters = None
    | I Int
    | F Float
    | II Int Int
    | IF Int Float
    | FF Float Float
    | F4 Float Float Float Float
    | LF [Float]
    | LF2 [Float] [Float]
    deriving (Eq,Show)
