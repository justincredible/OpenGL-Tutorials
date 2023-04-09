module Engine (run) where

import Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess)
import System.Win32.Info (SMSetting(..),sM_CXSCREEN,sM_CYSCREEN)
import System.Win32.Info.Computer (getSystemMetrics)

import Frame

data Engine = Engine Window Frame

run = do
    GLFW.init
    
    let width = 800
        height = 600
    window <- openWindow "Tutorial 4" width height
    
    frame <- Frame.initialize window width height
    loop frame
    
    GLFW.destroyWindow window
    GLFW.terminate
    where
    loop frame = do
        GLFW.pollEvents
        Frame.render frame
        loop frame

openWindow title width height = do
    GLFW.defaultWindowHints
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 6)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    GLFW.windowHint (GLFW.WindowHint'Decorated False)
    
    Just window <- createWindow width height title Nothing Nothing
    
    GLFW.makeContextCurrent (Just window)
    GLFW.setKeyCallback window (Just keyPressed)
    GLFW.setWindowCloseCallback window (Just closeWindow)
    
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    GLFW.setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    
    setCursorInputMode window CursorInputMode'Hidden
    
    return window
    
closeWindow window = do
    GLFW.destroyWindow window
    GLFW.terminate
    _ <- exitSuccess
    return ()

keyPressed window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = closeWindow window
keyPressed _ _ _ _ _ = return ()
