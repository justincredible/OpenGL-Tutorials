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
    window <- openWindow "Tutorial 4" width height
    
    glClearColor 0 0 0.4 0
    
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    
    vertexArray <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
    glBindVertexArray vertexArray

    shader <- compileAndLink "glsl/color.vertexshader" "glsl/color.fragmentshader"
    when (shader == 0) $ putStrLn "shader error"

    matrixID <- withArray0 0 (map castCharToCChar "MVP") (glGetUniformLocation shader)
    let mvp = buildModelViewPerspective
            0.785398163 (fromIntegral width/fromIntegral height) 0.1 100.0
            [4,3,3] [0,0,0] [0,1,0]
            1 1 1 0.785398163 0.785398163 0.785398163 1
    
    let triangle = [ 
            -1,-1,0,
            1,-1,0,
            0,1,0 ] :: [GLfloat]
        triVert = 3
        triclr = [
            1,0,0,
            1,0,0,
            1,0,0 ] :: [GLfloat]
        vertices = [
            -1,-1,-1,
            -1,-1,1,
            -1,1,1,
            1,1,-1,
            -1,-1,-1,
            -1,1,-1,
            1,-1,1,
            -1,-1,-1,
            1,-1,-1,
            1,1,-1,
            1,-1,-1,
            -1,-1,-1,
            -1,-1,-1,
            -1,1,1,
            -1,1,-1,
            1,-1,1,
            -1,-1,1,
            -1,-1,-1,
            -1,1,1,
            -1,-1,1,
            1,-1,1,
            1,1,1,
            1,-1,-1,
            1,1,-1,
            1,-1,-1,
            1,1,1,
            1,-1,1,
            1,1,1,
            1,1,-1,
            -1,1,-1,
            1,1,1,
            -1,1,-1,
            -1,1,1,
            1,1,1,
            -1,1,1,
            1,-1,1 ] :: [GLfloat]
        numVert = 36
        colours = [
            0.583,0.771,0.014,
            0.609,0.115,0.436,
            0.327,0.483,0.844,
            0.822,0.569,0.201,
            0.435,0.602,0.223,
            0.310,0.747,0.185,
            0.597,0.770,0.761,
            0.559,0.436,0.730,
            0.359,0.583,0.152,
            0.483,0.596,0.789,
            0.559,0.861,0.639,
            0.195,0.548,0.859,
            0.014,0.184,0.576,
            0.771,0.328,0.970,
            0.406,0.615,0.116,
            0.676,0.977,0.133,
            0.971,0.572,0.833,
            0.140,0.616,0.489,
            0.997,0.513,0.064,
            0.945,0.719,0.592,
            0.543,0.021,0.978,
            0.279,0.317,0.505,
            0.167,0.620,0.077,
            0.347,0.857,0.137,
            0.055,0.953,0.042,
            0.714,0.505,0.345,
            0.783,0.290,0.734,
            0.722,0.645,0.174,
            0.302,0.455,0.848,
            0.225,0.587,0.040,
            0.517,0.713,0.338,
            0.053,0.959,0.120,
            0.393,0.621,0.362,
            0.673,0.211,0.457,
            0.820,0.883,0.371,
            0.982,0.099,0.879 ] :: [GLfloat]
        numClr = numVert
    
    vertexBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    let vertSize = 3*sizeOf (head vertices)
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVert*vertSize) ptr GL_STATIC_DRAW
    
    colorBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER colorBuffer
    let clrSize = 3*sizeOf (head colours)
    withArray colours $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numClr*clrSize) ptr GL_STATIC_DRAW
    
    triBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER triBuffer
    let triSize = 3*sizeOf (head triangle)
    withArray triangle $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ triVert*triSize) ptr GL_STATIC_DRAW
    
    tcBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER tcBuffer
    let tcSize = 3*sizeOf (head triclr)
    withArray triclr $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ triVert*tcSize) ptr GL_STATIC_DRAW
    
    loop window shader vertexBuffer colorBuffer triBuffer tcBuffer matrixID mvp
    
    with colorBuffer $ \ptr ->
        glDeleteBuffers 1 ptr
    with vertexBuffer $ \ptr ->
        glDeleteBuffers 1 ptr
    with vertexArray $ \ptr ->
        glDeleteVertexArrays 1 ptr    
    glDeleteProgram shader
    
    destroyWindow window
    terminate
    where
    loop window shader vertexBuffer colorBuffer triBuffer tcBuffer matrixID mvp = do
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram shader

        withArray (concat mvp) $ glUniformMatrix4fv matrixID 1 GL_FALSE

        glEnableVertexAttribArray 0
        glEnableVertexAttribArray 1
        
        glBindBuffer GL_ARRAY_BUFFER vertexBuffer
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
        
        glBindBuffer GL_ARRAY_BUFFER colorBuffer
        glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE 0 nullPtr
        
        glDrawArrays GL_TRIANGLES 0 36

        withArray (concat . matmult4 mvp $ translate4 0 0 1.5) $ glUniformMatrix4fv matrixID 1 GL_FALSE

        glBindBuffer GL_ARRAY_BUFFER triBuffer
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
        
        glBindBuffer GL_ARRAY_BUFFER tcBuffer
        glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE 0 nullPtr
        
        glDrawArrays GL_TRIANGLES 0 3
        
        glDisableVertexAttribArray 1
        glDisableVertexAttribArray 0
        
        swapBuffers window
        
        pollEvents
        
        escape <- getKey window Key'Escape
        quit <- windowShouldClose window
        when (escape /= KeyState'Pressed && not quit) $ loop window shader vertexBuffer colorBuffer triBuffer tcBuffer matrixID mvp

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

buildModelViewPerspective fov aspect near far position lookat up tx ty tz rx ry rz scale =
    matmult4 (perspective4 fov aspect near far) . matmult4 (view4 position lookat up) $ identity4
