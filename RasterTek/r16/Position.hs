module Position (Position(..),turn) where

data Position = Position {
    getFrameTime :: Float,
    getRotationY :: Float,
    getLeftSpeed :: Float,
    getRightSpeed :: Float }
    deriving (Eq,Show)

turn left right position = let
    lts = if left
        then min (getFrameTime position*0.0015) (getLeftSpeed position + getFrameTime position*0.0001)
        else max 0 (getLeftSpeed position - getFrameTime position*0.00005)
    rts = if right
        then min (getFrameTime position*0.0015) (getRightSpeed position + getFrameTime position*0.0001)
        else max 0 (getRightSpeed position - getFrameTime position*0.00005)
    rotY = case (getRotationY position - lts < -pi, getRotationY position + rts > pi) of
        (True,_) -> -pi
        (_,True) -> pi
        _ -> getRotationY position - lts + rts
    in
    position { getLeftSpeed = lts, getRightSpeed = rts, getRotationY = rotY }
