module Engine (run) where

import Control.Monad (forever)
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess)
import System.Win32.Info (SMSetting(..),sM_CXSCREEN,sM_CYSCREEN)
import System.Win32.Info.Computer (getSystemMetrics)

import Frame

run title width height = do
    GLFW.init
    
    GLFW.defaultWindowHints
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 5)
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
    
    loop window
    
    GLFW.destroyWindow window
    GLFW.terminate
    where
    loop win = do
        GLFW.pollEvents
        
        Frame.render win
        
        loop win

keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = closeWindow win
keyPressed _ _ _ _ _ = return ()
    
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()
