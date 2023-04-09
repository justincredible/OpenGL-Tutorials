module Input (Input,initialize,mouseLocation) where

import Graphics.UI.GLFW

import Flow.Render

data Input = Input {
    getWindow :: Window,
    getScreenSize :: (Int,Int),
    getMousePos :: (Int,Int) }
    deriving (Eq,Show)

initialize window width height = Input window (width,height) (0,0)

readKeyboard window = sequence $ map (getKey window . toEnum) [0..120] -- [(Key'Unknown)..(Key'Menu)]

readMouse window = getCursorPos window

instance Render Input where
    render input = do
        --keyboard <- readKeyboard (getWindow input)
        (x,y) <- readMouse (getWindow input)
        let mouse = (round x, round y)
        
        return . (,) True $ input { getMousePos = mouse }

mouseLocation = getMousePos
