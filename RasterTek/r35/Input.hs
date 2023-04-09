module Input (Input,initialize,mouseLocation,leftDown,rightDown) where

import Graphics.UI.GLFW

import Flow.Render

data Input = Input {
    getWindow :: Window,
    getScreenSize :: (Int,Int),
    getMousePos :: (Int,Int),
    getLeftDown :: Bool,
    getRightDown :: Bool }
    deriving (Eq,Show)

initialize window width height = Input window (width,height) (0,0) False False

readKeyboard window = sequence $ map (getKey window . toEnum) [0..120] -- [(Key'Unknown)..(Key'Menu)]

readMouse window = getCursorPos window

instance Render Input where
    render input = do
        (x,y) <- readMouse (getWindow input)
        let mouse = (round x, round y)
        
        left <- getKey (getWindow input) Key'Left
        right <- getKey (getWindow input) Key'Right
        
        return . (,) True $ input {
            getMousePos = mouse,
            getLeftDown = left == KeyState'Pressed,
            getRightDown = right == KeyState'Pressed }

mouseLocation = getMousePos
leftDown = getLeftDown
rightDown = getRightDown
