module Engine (run) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.GL
import System.Exit
import System.Win32.Info
import System.Win32.Info.Computer

import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Input
import Frame
import Timer

data Engine = Engine {
    getWindow :: Window,
    getFrame :: Maybe Frame,
    getInput :: Input,
    getTimer :: Timer }
    deriving (Eq, Show)

run = do
    GLFW.init
    
    let width = 800; height = 600
    
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    
    window <- openWindow "Tutorial 18" width height
    
    setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    setCursorPos window (fromIntegral $ quot width 2) (fromIntegral $ quot height 2)
    
    let input = Input.initialize window screenW screenH
    
    (success, frame) <- Frame.initialize window (fromIntegral width) (fromIntegral height)
    
    (_,timer) <- Timer.initialize
    
    if not success
    then closeWindow frame window
    else do
        windowCallbacks window
        
        engine <- loop $ Engine window frame input timer
        
        closeWindow frame window
    where
    loop engine = do
        pollEvents
        
        (_,timer) <- render (getTimer engine)
        
        (_,input) <- render (getInput engine)
        
        (success, frame) <- update (getFrame engine) None >>= (render . snd)

        quit <- windowShouldClose (getWindow engine)
        
        if success && not quit
        then loop engine {
            getInput = input,
            getFrame = frame,
            getTimer = timer }
        else return engine {
            getInput = input,
            getFrame = frame,
            getTimer = timer }

openWindow title width height = do
    defaultWindowHints
    windowHint (WindowHint'ContextVersionMajor 4)
    windowHint (WindowHint'ContextVersionMinor 6)
    windowHint (WindowHint'Resizable False)
    windowHint (WindowHint'Decorated False)
    
    Just window <- createWindow width height title Nothing Nothing
    
    makeContextCurrent (Just window)
    
    setStickyKeysInputMode window StickyKeysInputMode'Enabled
    setCursorInputMode window CursorInputMode'Hidden
    
    return window
    
closeWindow frame window = do
    shutdown frame
    destroyWindow window
    terminate

windowCallbacks window = do
    setKeyCallback window (Just keyPressed)
    setWindowCloseCallback window . Just $ flip setWindowShouldClose True

keyPressed window Key'Escape _ KeyState'Pressed _ = setWindowShouldClose window True
keyPressed _ _ _ _ _ = return ()
