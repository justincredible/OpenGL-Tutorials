module Engine (run) where

import Control.Monad
import Graphics.UI.GLFW as GLFW
import System.Exit
import System.Win32.Info
import System.Win32.Info.Computer

import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Input
import Frame
import Audio

data Engine = Engine {
    getWindow :: Window,
    getInput :: Input,
    getSound :: Audio,
    getFrame :: Maybe Frame }
    deriving (Eq, Show)

run = do
    GLFW.init
    
    let width = 800; height = 600
    
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    
    window <- openWindow "Tutorial 14" width height
    
    setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    setCursorPos window (fromIntegral $ quot width 2) (fromIntegral $ quot height 2)
    
    let input = Input.initialize window screenW screenH
    
    (success, frame) <- Frame.initialize window (fromIntegral width) (fromIntegral height)
    
    (_,audio) <- Audio.initialize "asset/sound01.wav"
    
    if not success
    then closeWindow audio frame window
    else do
        windowCallbacks window frame
        
        engine <- loop $ Engine window input audio frame
        
        closeWindow audio frame window
    where
    loop engine@(Engine window input audio (Just frame)) = do
        pollEvents
        
        (_,input) <- render (getInput engine)
        
        let (mx,my) = mouseLocation input
        
        (success, frame) <- join . fmap (render.snd) $ update (getFrame engine) (II mx my)
        
        quit <- windowShouldClose (getWindow engine)
        
        if success && not quit
        then loop engine {
            getInput = input,
            getFrame = frame }
        else return engine {
            getInput = input,
            getFrame = frame }

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
    
closeWindow audio frame window = do
    shutdown audio
    shutdown frame
    destroyWindow window
    terminate

windowCallbacks window frame = do
    setKeyCallback window (Just keyPressed)
    setWindowCloseCallback window . Just $ flip setWindowShouldClose True

keyPressed window Key'Escape _ KeyState'Pressed _ = setWindowShouldClose window True
keyPressed _ _ _ _ _ = return ()
