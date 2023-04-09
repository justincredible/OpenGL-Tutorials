module Position (Position,initialize,getPosition,getRotation,getFrameTime,move) where

data Position = Position {
    getPosition :: [Float],
    getRotation :: [Float],
    getFrameTime :: Float,
    getForwardSpeed :: Float,
    getBackwardSpeed :: Float,
    getRisingSpeed :: Float,
    getFallingSpeed :: Float,
    getLeftSpeed :: Float,
    getRightSpeed :: Float,
    getUpSpeed :: Float,
    getDownSpeed :: Float }
    deriving (Eq,Show)

initialize = Position [0,0,0] [0,0,0] 0 0 0 0 0 0 0 0 0

move forth back rise fall left right up down position = let
    forthspeed = if forth
        then min (getFrameTime position*0.03) (getForwardSpeed position + getFrameTime position*0.001)
        else max 0 (getForwardSpeed position - getFrameTime position*0.0007)
    backspeed = if back
        then min (getFrameTime position*0.03) (getBackwardSpeed position + getFrameTime position*0.001)
        else max 0 (getBackwardSpeed position - getFrameTime position*0.0007)
    risespeed = if rise
        then min (getFrameTime position*0.03) (getRisingSpeed position + getFrameTime position*0.003)
        else max 0 (getRisingSpeed position - getFrameTime position*0.002)
    fallspeed = if fall
        then min (getFrameTime position*0.03) (getFallingSpeed position + getFrameTime position*0.003)
        else max 0 (getFallingSpeed position - getFrameTime position*0.002)
    leftspeed = if left
        then min (getFrameTime position*0.0015) (getLeftSpeed position + getFrameTime position*0.0003)
        else max 0 (getLeftSpeed position - getFrameTime position*0.0001)
    rightspeed = if right
        then min (getFrameTime position*0.0015) (getRightSpeed position + getFrameTime position*0.0003)
        else max 0 (getRightSpeed position - getFrameTime position*0.0001)
    upspeed = if up
        then min (getFrameTime position*0.0015) (getUpSpeed position + getFrameTime position*0.0001)
        else max 0 (getUpSpeed position - getFrameTime position*0.0001)
    downspeed = if down
        then min (getFrameTime position*0.0015) (getDownSpeed position + getFrameTime position*0.0001)
        else max 0 (getDownSpeed position - getFrameTime position*0.0001)
    rotationY = getRotation position !! 1 - leftspeed + rightspeed
    rotationY' = if rotationY < 0
        then rotationY + 2*pi
        else if rotationY > 2*pi
            then rotationY - 2*pi
            else rotationY
    rotationX = max (-pi/2) (min (pi/2) $ getRotation position !! 0 + downspeed - upspeed)
    in
    position {
        getPosition = [
            (head . getPosition) position + sin rotationY'*(forthspeed - backspeed),
            (head . tail . getPosition) position + risespeed - fallspeed,
            (last . getPosition) position + cos rotationY'*(forthspeed - backspeed) ],
        getRotation = [rotationX, rotationY', 0],
        getForwardSpeed = forthspeed,
        getBackwardSpeed = backspeed,
        getRisingSpeed = risespeed,
        getFallingSpeed = fallspeed,
        getLeftSpeed = leftspeed,
        getRightSpeed = rightspeed,
        getUpSpeed = upspeed,
        getDownSpeed = downspeed }
