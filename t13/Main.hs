import Control.Monad
import Data.Foldable
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
    (window,(scrWidth, scrHeight)) <- openWindow "Tutorial 8" width height
    
    {-debugCallback <- GLFW.getWindowOpenGLDebugContext window >>= \confirmed -> do
        funptr <- makeGLDEBUGPROC debugOutputCallback
        
        if confirmed
        then do
            putStrLn "The OpenGL implementation provides debug output. Let's use it !\n"
            glDebugMessageCallbackARB funptr nullPtr
            glEnable GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB
        else
            putStrLn "ARB_debug_output unavailable. You have to use glGetError() and/or gDebugger to catch mistakes.\n"

        return funptr-}
    
    glClearColor 0 0 0.4 0
    
    glEnable  GL_DEPTH_TEST
    glDepthFunc GL_LESS 
    
    glEnable GL_CULL_FACE
    
    vertexArray <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
    glBindVertexArray vertexArray

    shader <- compileAndLink "glsl/bump.vertex" "glsl/bump.fragment"
    when (shader == 0) $ putStrLn "shader error"

    uniforms <- sequence
        . map (flip (withArray0 0) $ glGetUniformLocation shader)
        $ map (map castCharToCChar) ["projection","view","model","baseview"]
        
    lightpos <- withArray0 0 (map castCharToCChar "lightpos") $ glGetUniformLocation shader
    
    texlocns <- sequence
        . map (flip (withArray0 0) $ glGetUniformLocation shader)
        $ map (map castCharToCChar) ["diffuse","normal","specular"]

    textures <- sequence $ zipWith ($)
        [loadDDS,loadBMP_demo,loadDDS]
        ["asset/diffuse.dds","asset/normal.bmp","asset/specular.dds"]
    
    model <- fmap (indexVBO_TBN . computeTangentBasis) . loadObj $ "asset/cylinder.obj"

    buffers <- sequence . map (bufferData GL_ARRAY_BUFFER) $ init model

    let indices = map (map round) (last model) :: [[GLushort]]
    (_,elemBuffer) <- bufferData GL_ELEMENT_ARRAY_BUFFER indices

    control <- newControl window (fromIntegral scrWidth) (fromIntegral scrHeight)
    
    loop window control shader (fromIntegral . length $ indices) buffers elemBuffer texlocns textures uniforms lightpos
    
    sequence_ $ map (flip with (glDeleteBuffers 1) . snd) buffers
    
    sequence_ $ zipWith with
        [elemBuffer, vertexArray]
        (map ($ 1) [glDeleteBuffers,glDeleteVertexArrays])
    
    sequence_ $ map (flip with $ glDeleteTextures 1) textures

    glDeleteProgram shader
    
    --freeHaskellFunPtr debugCallback
    
    GLFW.destroyWindow window
    GLFW.terminate
    where
    setUniformMatrix id val = case length val of
        4 -> withArray val $ glUniformMatrix2fv id 1 GL_FALSE
        9 -> withArray val $ glUniformMatrix3fv id 1 GL_FALSE
        16 -> withArray val $ glUniformMatrix4fv id 1 GL_FALSE
        _ -> return ()
    
    setTexture i location texture = do
        glActiveTexture (GL_TEXTURE0 + fromIntegral i)
        glBindTexture GL_TEXTURE_2D texture
        glUniform1i location i
    
    setBuffer a (c,b) = do
        glBindBuffer GL_ARRAY_BUFFER b
        glVertexAttribPointer a (fromIntegral c) GL_FLOAT GL_FALSE 0 nullPtr

    bufferData buftyp xs = do
        buffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
        glBindBuffer buftyp buffer
        withArray (concat xs) $ \ptr ->
            glBufferData buftyp (ecs xs) ptr GL_STATIC_DRAW
        return (length . head $ xs, buffer)
        
    -- elements*components*size
    ecs :: Storable a => [[a]] -> CPtrdiff
    ecs = fromIntegral . ((*) . length <*> ((*).length.head <*> sizeOf.head.head))
    
    loop window control shader indices buffers element texlocns textures uniforms lightpos = do
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram shader
        
        control' <- computeMatricesFromInputs control
        
        sequence_ $ zipWith setUniformMatrix uniforms
            (map concat [getProjection control',getView control',identity4, init . map init $ getView control'])

        withArray [0,0,4] $ glUniform3fv lightpos 1
        
        sequence_ $ zipWith3 setTexture [0..] texlocns textures

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
            loop window control' shader indices buffers element texlocns textures uniforms lightpos

openWindow title width height = do
    GLFW.defaultWindowHints
    -- Set up PixelFormat in GLFW
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    GLFW.windowHint (GLFW.WindowHint'Decorated False)
    GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True); 
    
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

--debugOutputCallback :: GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()
debugOutputCallback source errtyp id severity errlen message userparam = do
    putStrLn "OpenGL Debug Output message : "

    case source of
        GL_DEBUG_SOURCE_API_ARB -> putStrLn "Source : API; "
        GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB -> putStrLn "Source : WINDOW_SYSTEM; "
        GL_DEBUG_SOURCE_SHADER_COMPILER_ARB -> putStrLn "Source : SHADER_COMPILER; "
        GL_DEBUG_SOURCE_THIRD_PARTY_ARB -> putStrLn "Source : THIRD_PARTY; "
        GL_DEBUG_SOURCE_APPLICATION_ARB -> putStrLn "Source : APPLICATION; "
        GL_DEBUG_SOURCE_OTHER_ARB -> putStrLn "Source : OTHER; "

    case errtyp of
        GL_DEBUG_TYPE_ERROR_ARB -> putStrLn "Type : ERROR; "
        GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB -> putStrLn "Type : DEPRECATED_BEHAVIOR; "
        GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB -> putStrLn "Type : UNDEFINED_BEHAVIOR; "
        GL_DEBUG_TYPE_PORTABILITY_ARB -> putStrLn "Type : PORTABILITY; "
        GL_DEBUG_TYPE_PERFORMANCE_ARB -> putStrLn "Type : PERFORMANCE; "
        GL_DEBUG_TYPE_OTHER_ARB -> putStrLn "Type : OTHER; "

    case severity of
        GL_DEBUG_SEVERITY_HIGH_ARB -> putStrLn "Severity : HIGH; "
        GL_DEBUG_SEVERITY_MEDIUM_ARB -> putStrLn "Severity : MEDIUM; "
        GL_DEBUG_SEVERITY_LOW_ARB -> putStrLn "Severity : LOW; "
        GL_DEBUG_SEVERITY_NOTIFICATION -> putStrLn "Severity : NOTIFICATION; "

    peekArray (fromIntegral errlen) message >>=
        putStrLn . ("Message : " ++) . map castCCharToChar
