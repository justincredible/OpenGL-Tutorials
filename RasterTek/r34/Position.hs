module Position (Position,initialize,getPosition,getRotation,getFrameTime,turn) where

data Position = Position {
    getPosition :: [Float],
    getRotation :: [Float],
    getFrameTime :: Float,
    getLeftSpeed :: Float,
    getRightSpeed :: Float }
    deriving (Eq,Show)

initialize = Position [0,0,0] [0,0,0] 0 0 0

turn left right position = let
    ls = if left
        then min (getFrameTime position*0.005) (getLeftSpeed position + getFrameTime position*0.15)
        else max 0 (getLeftSpeed position - getFrameTime position*0.0035)
    rs = if right
        then min (getFrameTime position*0.005) (getRightSpeed position + getFrameTime position*0.15)
        else max 0 (getRightSpeed position - getFrameTime position*0.0035)
    -- rotation.y is always 0
    in
    position {
        getPosition = [
            (head . getPosition) position - ls + rs,
            head . tail . getPosition $ position,
            last . getPosition $ position ],
        getLeftSpeed = ls,
        getRightSpeed = rs }
