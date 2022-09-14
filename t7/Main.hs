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

import Control
import Maths
import Object
import ShaderCompilinker
import Texture

main = do
    glfwInit <- Graphics.UI.GLFW.init
    
    when (not glfwInit) $ putStrLn "Failed to initialize GLFW\n"
    
    let width = 800
        height = 600
    (window,(scrWidth, scrHeight)) <- openWindow "Tutorial 7" width height
    
    glClearColor 0 0 0.4 0
    
    glEnable  GL_DEPTH_TEST
    glDepthFunc GL_LESS 
    
    glEnable GL_CULL_FACE
    
    vertexArray <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
    glBindVertexArray vertexArray

    shader <- compileAndLink "glsl/Simple.vertexshader" "glsl/Simple.fragmentshader"
    when (shader == 0) $ putStrLn "shader error"

    matrixID <- withArray0 0 (map castCharToCChar "MVP") (glGetUniformLocation shader)

    texture <- loadDDS "asset/uvmap.dds"
    
    textureID <- withArray0 0 (map castCharToCChar "sample") (glGetUniformLocation shader)
    
    (vertices,texcoords,_) <- loadObj "asset/cube.obj"
    let numVert = length vertices
        numCrd = numVert

    vertexBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    let vertSize = (length.head) vertices*(sizeOf.head.head)vertices
    withArray (concat vertices) $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVert*vertSize) ptr GL_STATIC_DRAW
    
    coordBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER coordBuffer
    let crdSize = (length.head) texcoords*(sizeOf.head.head)texcoords
    withArray (concat texcoords) $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numCrd*crdSize) ptr GL_STATIC_DRAW

    control <- newControl window (fromIntegral scrWidth) (fromIntegral scrHeight)
    
    loop window control shader vertexBuffer coordBuffer texture textureID matrixID
    
    sequence_ $ zipWith ($)
        (map with [vertexBuffer, coordBuffer, vertexArray, texture])
        (map ($ 1) [glDeleteBuffers, glDeleteBuffers, glDeleteVertexArrays, glDeleteTextures])
    glDeleteProgram shader
    
    destroyWindow window
    terminate
    where
    loop window control shader vertexBuffer coordBuffer texture textureID matrixID = do
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram shader
        
        control' <- computeMatricesFromInputs control
        let mvp = matmult4 (getProjection control') (getView control')
        withArray (concat mvp) $ glUniformMatrix4fv matrixID 1 GL_FALSE
        
        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D texture
        glUniform1i textureID 0

        sequence_ $ map glEnableVertexAttribArray [0..1]
        
        glBindBuffer GL_ARRAY_BUFFER vertexBuffer
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
        
        glBindBuffer GL_ARRAY_BUFFER coordBuffer
        glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 0 nullPtr
        
        glDrawArrays GL_TRIANGLES 0 36
        
        sequence_ $ map glDisableVertexAttribArray [0..1]
        
        swapBuffers window
        
        pollEvents
        
        escape <- getKey window Key'Escape
        quit <- windowShouldClose window
        when (escape /= KeyState'Pressed && not quit) $ loop window control' shader vertexBuffer coordBuffer texture textureID matrixID

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
    
    -- Windows code
    {--}
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    {--}
    
    setCursorInputMode window CursorInputMode'Disabled
    setCursorPos window (fromIntegral $ quot screenW 2) (fromIntegral $ quot screenH 2)
    
    return (window,(screenW,screenH))
