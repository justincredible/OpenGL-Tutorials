import Control.Monad
import Graphics.GL
import Graphics.UI.GLFW
import System.Exit
import System.Win32.Info
import System.Win32.Info.Computer

main = do
    glfwInit <- Graphics.UI.GLFW.init
    
    when (not glfwInit) (putStrLn "Failed to initialize GLFW\n")
    
    let width = 800
        height = 600
    window <- openWindow "Tutorial 1" width height
    
    windowCallbacks window
        
    glClearColor 0 0 0 1
    loop window
    
    destroyWindow window
    terminate
    where
    loop window = do
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        swapBuffers window
        
        pollEvents
        
        escape <- getKey window Key'Escape
        quit <- windowShouldClose window
        when (escape /= KeyState'Pressed && not quit) $ loop window

openWindow title width height = do
    defaultWindowHints
    -- Set up PixelFormat in GLFW
    windowHint (WindowHint'ContextVersionMajor 4)
    windowHint (WindowHint'ContextVersionMinor 6)
    windowHint (WindowHint'Resizable False)
    windowHint (WindowHint'Decorated False)
    
    Just window <- createWindow width height title Nothing Nothing
    makeContextCurrent (Just window)
    
    setStickyKeysInputMode window StickyKeysInputMode'Enabled
    
    setCursorInputMode window CursorInputMode'Hidden

    -- Windows code
    {--}
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    {--}
    
    return window

windowCallbacks window = setWindowCloseCallback window . Just $ closeWindow
    
closeWindow window = do
    destroyWindow window
    terminate
    _ <- exitSuccess
    return ()
