import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import Graphics.UI.GLFW
import System.Exit
import System.Win32.Info
import System.Win32.Info.Computer

import Maths
import ShaderCompilinker

main = do
    glfwInit <- Graphics.UI.GLFW.init
    
    when (not glfwInit) $ putStrLn "Failed to initialize GLFW\n"
    
    let width = 800
        height = 600
    window <- openWindow "Tutorial 3" width height
    
    glClearColor 0 0 0.4 0
    
    vertexArray <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
    glBindVertexArray vertexArray

    shader <- compileAndLink "glsl/Simple.vertexshader" "glsl/Simple.fragmentshader"
    when (shader == 0) $ putStrLn "shader error"

    matrixID <- withArray0 0 (map castCharToCChar "MVP") (glGetUniformLocation shader)
    let mvp = buildModelViewPerspective
            0.78539816 (fromIntegral width/fromIntegral height) 0.1 100.0
            [4,3,3] [0,0,0] [0,1,0]
            1
    
    let vertices = [ 
            -1.0, -1.0, 0.0,
            1.0, -1.0, 0.0,
            0.0,  1.0, 0.0 ] :: [GLfloat]
        numVert = 3
    
    vertexBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    let vertSize = sizeOf (head vertices)*quot (foldr ((+).const 1) 0 vertices) numVert
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVert*vertSize) ptr GL_STATIC_DRAW
    
    loop window shader vertexArray matrixID mvp
        
    with vertexBuffer $ \ptr ->
        glDeleteBuffers 1 ptr
    with vertexArray $ \ptr ->
        glDeleteVertexArrays 1 ptr    
    glDeleteProgram shader
    
    destroyWindow window
    terminate
    where
    loop window shader vertexArray matrixID mvp = do
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram shader

        withArray (concat mvp) $ glUniformMatrix4fv matrixID 1 GL_FALSE

        glEnableVertexAttribArray 0
        
        glBindVertexArray vertexArray
    
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
        
        glDrawArrays GL_TRIANGLES 0 3
        
        glDisableVertexAttribArray 0
        
        swapBuffers window
        
        pollEvents
        
        escape <- getKey window Key'Escape
        quit <- windowShouldClose window
        when (escape /= KeyState'Pressed && not quit) $ loop window shader vertexArray matrixID mvp

openWindow title width height = do
    defaultWindowHints
    -- Set up PixelFormat in GLFW
    windowHint (WindowHint'ContextVersionMajor 4)
    windowHint (WindowHint'ContextVersionMinor 6)
    windowHint (WindowHint'Resizable False)
    windowHint (WindowHint'Decorated False)
    
    Just window <- createWindow width height title Nothing Nothing
    makeContextCurrent (Just window)
    
    -- No GLEW
    
    setStickyKeysInputMode window StickyKeysInputMode'Enabled
    
    setCursorInputMode window CursorInputMode'Hidden

    -- Windows code
    {--}
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    {--}
    
    return window
