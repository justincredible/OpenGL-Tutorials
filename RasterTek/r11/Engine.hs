module Engine (run) where

import Control.Monad
import Graphics.UI.GLFW
import System.Exit
import System.Win32.Info
import System.Win32.Info.Computer

import Frame

data Engine = Engine {
    getWindow :: Window,
    getFrame :: Maybe Frame }
    deriving (Eq, Show)

run = do
    Graphics.UI.GLFW.init
    
    let width = 800
        height = 600
    
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    
    window <- openWindow "Tutorial 11" width height
    
    setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    
    (success, frame) <- Frame.initialize window (fromIntegral width) (fromIntegral height)
    
    when (not success) $ closeWindow frame window
    
    windowCallbacks window frame
    
    loop (Engine window frame)
    
    shutdown frame
    destroyWindow window
    terminate
    where
    loop engine@(Engine _ (Just frame)) = do
        pollEvents
        
        (success, frame') <- Frame.render frame
        
        when success . loop $ engine { getFrame = Just frame' }

openWindow title width height = do
    defaultWindowHints
    windowHint (WindowHint'ContextVersionMajor 4)
    windowHint (WindowHint'ContextVersionMinor 6)
    windowHint (WindowHint'Resizable False)
    windowHint (WindowHint'Decorated False)
    
    Just window <- createWindow width height title Nothing Nothing
    
    makeContextCurrent (Just window)
    
    setCursorInputMode window CursorInputMode'Hidden
    
    return window

windowCallbacks window frame = do
    setKeyCallback window . Just $ keyPressed frame
    setWindowCloseCallback window . Just $ closeWindow frame
    
closeWindow frame window = do
    shutdown frame
    destroyWindow window
    terminate
    _ <- exitSuccess
    return ()

keyPressed frame window Key'Escape _ KeyState'Pressed _ = closeWindow frame window
keyPressed _ _ _ _ _ _ = return ()
