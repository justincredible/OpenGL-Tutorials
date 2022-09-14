module Controls (defaultControls, computeMatricesFromInputs, getView, getProjection) where

import GHC.Float
import Graphics.GL
import Graphics.UI.GLFW

import Maths

data Controls = Controls {
    getWindow :: Window,
    getScreenWidth :: GLuint,
    getScreenHeight :: GLuint,
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

defaultControls window scrWidth scrHeight = do
    Just time <- getTime
    return $ Controls
        window scrWidth scrHeight
        --(view4 [0,0,5] [0,0,0] [0,1,0]) (perspective4 (pi/4) (4/3) 0.1 100.0)
        identity4 identity4
        [0,0,5]
        pi 0
        (pi/4)
        time

computeMatricesFromInputs controls = do
    Just currentTime <- getTime
    let deltaTime = currentTime - getLastTime controls
    
    (posx, posy) <- getCursorPos (getWindow controls)
    
    setCursorPos (getWindow controls) (fromIntegral $ getScreenWidth controls `quot` 2) (fromIntegral $ getScreenHeight controls `quot` 2)
    
    let horizontalAngle = getHAngle controls + double2Float (mouseSpeed * (fromIntegral (getScreenWidth controls `quot` 2) - posx))
        horizontalAngle' = if horizontalAngle <= 0 then horizontalAngle + 2*pi else if horizontalAngle > 2*pi then horizontalAngle - 2*pi else horizontalAngle
        verticalAngle = getVAngle controls + double2Float (mouseSpeed * (fromIntegral (getScreenHeight controls `quot` 2) - posy))
        verticalAngle' = if verticalAngle <= -pi/2 then -pi/2 else if verticalAngle > pi/2 then pi/2 else verticalAngle
        direction = [ cos verticalAngle'*sin horizontalAngle', sin verticalAngle', cos verticalAngle'*cos horizontalAngle' ]
        right = [ sin (horizontalAngle' - pi/2), 0, cos (horizontalAngle' - pi/2) ]
        up = cross right direction
    
    let position = getPosition controls
    keydown <- getKey (getWindow controls) Key'Up
    let position' = if keydown == KeyState'Pressed then add position $ map (* (double2Float deltaTime * speed)) direction  else position
    keydown <- getKey (getWindow controls) Key'Down
    let position = if keydown == KeyState'Pressed then minus position' $ map (* (double2Float deltaTime * speed)) direction else position'
    keydown <- getKey (getWindow controls) Key'Left
    let position' = if keydown == KeyState'Pressed then minus position $ map (* (double2Float deltaTime * speed)) right else position
    keydown <- getKey (getWindow controls) Key'Right
    let position = if keydown == KeyState'Pressed then add position' $ map (* (double2Float deltaTime * speed)) right else position'

    let projection = perspective4 (getFOV controls) (4/3) 0.1 100.0
        view = view4 position (add position direction) up

    return $ controls {
        getView = view,
        getProjection = projection,
        getPosition = position,
        getHAngle = horizontalAngle',
        getVAngle = verticalAngle',
        getLastTime = currentTime }
