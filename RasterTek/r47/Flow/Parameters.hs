module Flow.Parameters (Parameters(..)) where

import Data.Int

data Parameters = None
    | B Bool
    | I Int
    | F Float
    | II Int Int
    | IF Int Float
    | FF Float Float
    | BII Bool Int Int
    | F4 Float Float Float Float
    | LF [Float]
    | LF2 [Float] [Float]
    | ShaderTexture [Float] [Float] [Float] Int32
    | ShaderLight [Float] [Float] [Float] Int32 [Float] [Float] [Float] [Float] [Float] Float
    | ShaderBumpMap [Float] [Float] [Float] Int32 Int32 [Float] [Float]
    deriving (Eq,Show)
