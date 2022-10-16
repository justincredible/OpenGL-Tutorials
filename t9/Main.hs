import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.Win32.Info
import System.Win32.Info.Computer

import Control
import Maths
import Object
import ShaderCompilinker
import Texture

main = do
    glfwInit <- GLFW.init
    
    when (not glfwInit) $ putStrLn "Failed to initialize GLFW\n"
    
    let width = 800
        height = 600
    (window,(scrWidth, scrHeight)) <- openWindow "Tutorial 9" width height
    
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
    
    model <- fmap indexVBO . loadObj $ "asset/suzanne.obj"

    buffers@[(_,vertexBuffer),(_,coordBuffer),(_,normalBuffer)] <- sequence
        . map bufferData
        $ init model

    elemBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER elemBuffer
    let indices = map (map round) (last model) :: [[GLushort]]
    withArray (concat indices) $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (ecs indices) ptr GL_STATIC_DRAW

    control <- newControl window (fromIntegral scrWidth) (fromIntegral scrHeight)
    
    loop window control shader (fromIntegral . length $ indices) buffers elemBuffer textureID texture uniforms lightpos
    
    sequence_ $ zipWith ($)
        (map with [vertexBuffer, coordBuffer, normalBuffer, elemBuffer, vertexArray, texture])
        (map ($ 1) [glDeleteBuffers, glDeleteBuffers, glDeleteBuffers, glDeleteBuffers, glDeleteVertexArrays, glDeleteTextures])
    glDeleteProgram shader
    
    GLFW.destroyWindow window
    GLFW.terminate
    where
    setUniformMatrices id val = withArray val $ glUniformMatrix4fv id 1 GL_FALSE
    
    setBuffer a (c,b) = do
        glBindBuffer GL_ARRAY_BUFFER b
        glVertexAttribPointer a (fromIntegral c) GL_FLOAT GL_FALSE 0 nullPtr

    bufferData xs = do
        buffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
        glBindBuffer GL_ARRAY_BUFFER buffer
        withArray (concat xs) $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (ecs xs) ptr GL_STATIC_DRAW
        return (length . head $ xs, buffer)
        
    -- elements*components*size
    ecs :: Storable a => [[a]] -> CPtrdiff
    ecs = fromIntegral . ((*) . length <*> ((*).length.head <*> sizeOf.head.head))
    
    loop window control shader indices buffers element textureID texture uniforms lightpos = do
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
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER $ element
        
        glDrawElements GL_TRIANGLES indices GL_UNSIGNED_SHORT nullPtr
        
        sequence_ $ map (glDisableVertexAttribArray.fromIntegral) [0..length buffers - 1]
        
        GLFW.swapBuffers window
        
        GLFW.pollEvents
        
        escape <- GLFW.getKey window GLFW.Key'Escape
        quit <- GLFW.windowShouldClose window
        when (escape /= GLFW.KeyState'Pressed && not quit) $
            loop window control' shader indices buffers element textureID texture uniforms lightpos
    

openWindow title width height = do
    GLFW.defaultWindowHints
    -- Set up PixelFormat in GLFW
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    GLFW.windowHint (GLFW.WindowHint'Decorated False)
    
    Just window <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    
    GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled
    
    -- Windows code
    {--}
    screenW <- getSystemMetrics sM_CXSCREEN
    screenH <- getSystemMetrics sM_CYSCREEN
    GLFW.setWindowPos window (quot (screenW-width) 2) (quot (screenH-height) 2)
    {--}
    
    GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
    GLFW.setCursorPos window (fromIntegral $ quot screenW 2) (fromIntegral $ quot screenH 2)
    
    return (window,(screenW,screenH))
