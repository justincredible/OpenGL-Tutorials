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
    (window,(scrWidth, scrHeight)) <- openWindow "Tutorial 8" width height
    
    glClearColor 0 0 0.4 0
    
    glEnable  GL_DEPTH_TEST
    glDepthFunc GL_LESS 
    
    glEnable GL_CULL_FACE
    
    vertexArray <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
    glBindVertexArray vertexArray

    shader <- compileAndLink "glsl/shader.vertex" "glsl/shader.fragment"
    when (shader == 0) $ putStrLn "shader error"

    uniforms <- sequence
        . map (flip (withArray0 0) $ glGetUniformLocation shader)
        $ map (map castCharToCChar) ["projection","view","model"]
        
    lightpos <- withArray0 0 (map castCharToCChar "lightpos") $ glGetUniformLocation shader
    
    textureID <- withArray0 0 (map castCharToCChar "sample") $ glGetUniformLocation shader

    texture <- loadDDS "asset/uvsuzanne.dds"
    
    model <- loadObj "asset/suzanne.obj"

    buffers@[(_,vertexBuffer),(_,coordBuffer),(_,normalBuffer)] <- sequence . map bufferData $ model

    control <- newControl window (fromIntegral scrWidth) (fromIntegral scrHeight)
    
    loop window control shader (fromIntegral . length . head $ model) buffers textureID texture uniforms lightpos
    
    sequence_ $ zipWith ($)
        (map with [vertexBuffer, coordBuffer, normalBuffer, vertexArray, texture])
        (map ($ 1) [glDeleteBuffers, glDeleteBuffers, glDeleteBuffers, glDeleteVertexArrays, glDeleteTextures])
    glDeleteProgram shader
    
    destroyWindow window
    terminate
    where
    loop window control shader vertices buffers textureID texture uniforms lightpos = do
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram shader
        
        control' <- computeMatricesFromInputs control
        
        sequence_ $ zipWith setUniformMatrices uniforms
            (map concat [getProjection control',getView control',identity4])

        withArray [4,4,4] $ glUniform3fv lightpos 1
        
        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D texture
        glUniform1i textureID 0

        sequence_ $ map (glEnableVertexAttribArray.fromIntegral) [0..length buffers - 1]
        
        sequence_ $ zipWith setBuffer [0..] buffers
        
        glDrawArrays GL_TRIANGLES 0 vertices
        
        sequence_ $ map (glDisableVertexAttribArray.fromIntegral) [0..length buffers - 1]
        
        swapBuffers window
        
        pollEvents
        
        escape <- getKey window Key'Escape
        quit <- windowShouldClose window
        when (escape /= KeyState'Pressed && not quit) $ loop window control' shader vertices buffers textureID texture uniforms lightpos

    setBuffer a (c,b) = do
        glBindBuffer GL_ARRAY_BUFFER b
        glVertexAttribPointer a (fromIntegral c) GL_FLOAT GL_FALSE 0 nullPtr
    
    setUniformMatrices id val = withArray val $ glUniformMatrix4fv id 1 GL_FALSE
    
    bufferData xs = do
        buffer <- alloca $ \ptr -> do
            glGenBuffers 1 ptr
            peek ptr
        glBindBuffer GL_ARRAY_BUFFER buffer
        withArray (concat xs) $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (fromIntegral $ ((*).length<*>((*).length.head <*> fromIntegral.sizeOf.head.head)) xs) ptr GL_STATIC_DRAW
        return (length . head $ xs, buffer)

    --length = foldr ((+).const 1) 0
    

openWindow title width height = do
    defaultWindowHints
    -- Set up PixelFormat in GLFW
    windowHint (WindowHint'ContextVersionMajor 3)
    windowHint (WindowHint'ContextVersionMinor 3)
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
