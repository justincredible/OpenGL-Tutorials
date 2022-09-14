module Control (newControl, computeMatricesFromInputs, getView, getProjection, currentPosition, currentHAngle, currentVAngle) where

import GHC.Float
import Graphics.GL
import Graphics.UI.GLFW

import Maths

data Control = Control {
    getWindow :: Window,
    getView :: [[GLfloat]],
    getProjection :: [[GLfloat]],
    getPosition :: [GLfloat],
    getHAngle :: GLfloat,
    getVAngle :: GLfloat,
    getFOV :: GLfloat,
    getLastTime :: Double }
    deriving (Eq, Show)

mouseSpeed = 0.005
speed = 3.0

newControl window = do
    Just time <- getTime
    return $ Control window
        --(view4 [0,0,5] [0,0,0] [0,1,0]) (perspective4 (pi/4) (4/3) 0.1 100.0)
        identity4 identity4
        [0,0,5]
        pi 0
        (pi/4)
        time

computeMatricesFromInputs control = do
    Just currentTime <- getTime
    (width,height) <- getWindowSize . getWindow $ control
    let deltaTime = currentTime - getLastTime control
        hZero = fromIntegral $ width `quot` 2
        vZero = fromIntegral $ height `quot` 2
    
    (posx, posy) <- getCursorPos (getWindow control)
    setCursorPos (getWindow control) hZero vZero
    
    let horizontalAngle = getHAngle control + double2Float (mouseSpeed * (hZero - posx))
        hAngle = horizontalAngle + if horizontalAngle <= 0 then 2*pi else if horizontalAngle > 2*pi then -2*pi else 0
        verticalAngle = getVAngle control + double2Float (mouseSpeed * (vZero - posy))
        vAngle = if verticalAngle <= -pi/2 then -pi/2 else if verticalAngle > pi/2 then pi/2 else verticalAngle
        direction = [ cos vAngle*sin hAngle, sin vAngle, cos vAngle*cos hAngle ]
        right = [ sin (hAngle - pi/2), 0, cos (hAngle - pi/2) ]
        up = cross right direction
    
    let position = getPosition control
        scale = speed * double2Float deltaTime
    keydown <- getKey (getWindow control) Key'Up
    let position' = if keydown /= KeyState'Pressed then position else add position $ map (*scale) direction
    keydown <- getKey (getWindow control) Key'Down
    let position = if keydown /= KeyState'Pressed then position' else minus position' $ map (*scale) direction
    keydown <- getKey (getWindow control) Key'Left
    let position' = if keydown /= KeyState'Pressed then position else minus position $ map (*scale) right
    keydown <- getKey (getWindow control) Key'Right
    let position = if keydown /= KeyState'Pressed then position' else add position' $ map (*scale) right

    let projection = perspective4 (getFOV control) (4/3) 0.1 100.0
        view = view4 position (add position direction) up

    return $ control {
        getView = view,
        getProjection = projection,
        getPosition = position,
        getHAngle = hAngle,
        getVAngle = vAngle,
        getLastTime = currentTime }

currentPosition control = "pos: " ++ (filter (/= '"') . show . map (take 5.show) . getPosition) control
currentHAngle control = "Ha: " ++ (show . getHAngle) control
currentVAngle control = "Va: " ++ (show . getVAngle) control
