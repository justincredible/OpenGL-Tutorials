module Input (Input,initialize,mouseLocation,
    getUpPressed,
    getDownPressed,
    getLeftPressed,
    getRightPressed,
    getAPressed,
    getZPressed,
    getPgUpPressed,
    getPgDnPressed) where
 
import Graphics.UI.GLFW

import Flow.Render

data Input = Input { 
    getWindow :: Window, 
    getScreenSize :: (Int,Int),
    getMousePos :: (Int,Int),
    getUpPressed :: Bool,
    getDownPressed :: Bool,
    getLeftPressed :: Bool,
    getRightPressed :: Bool,
    getAPressed :: Bool,
    getZPressed :: Bool,
    getPgUpPressed :: Bool,
    getPgDnPressed :: Bool }
    deriving (Eq,Show)

initialize window width height = Input window (width,height) (0,0) False False False False False False False False

readKeyboard window = sequence $ map (getKey window . toEnum) [0..120] -- [(Key'Unknown)..(Key'Menu)]

readMouse window = getCursorPos window

instance Render Input where
    render input = do
        (x,y) <- readMouse (getWindow input)
        let mouse = (round x, round y)
        
        up <- getKey (getWindow input) Key'Up
        down <- getKey (getWindow input) Key'Down
        left <- getKey (getWindow input) Key'Left
        right <- getKey (getWindow input) Key'Right
        a <- getKey (getWindow input) Key'A
        z <- getKey (getWindow input) Key'Z
        pgup <- getKey (getWindow input) Key'PageUp
        pgdn <- getKey (getWindow input) Key'PageDown
        
        return . (,) True $ input {
            getMousePos = mouse,
            getUpPressed = up == KeyState'Pressed,
            getDownPressed = down == KeyState'Pressed,
            getLeftPressed = left == KeyState'Pressed,
            getRightPressed = right == KeyState'Pressed,
            getAPressed = a == KeyState'Pressed,
            getZPressed = z == KeyState'Pressed,
            getPgUpPressed = pgup == KeyState'Pressed,
            getPgDnPressed = pgdn == KeyState'Pressed }

mouseLocation = getMousePos
