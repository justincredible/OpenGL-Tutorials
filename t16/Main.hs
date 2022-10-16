import Control.Monad
import Data.Bifunctor
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
    (window,(scrWidth, scrHeight)) <- openWindow "Tutorial 16" width height

    -- uncomment freeHaskellFunPtr call below with this code
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

    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
    glBindVertexArray vertexArray

    depthshader <- compileAndLink "glsl/depthrender.vertex" "glsl/depthrender.fragment"
    
    depthlocn <- withArray0 0 (map castCharToCChar "depthMVP") $ glGetUniformLocation depthshader
    
    texture <- loadDDS "asset/uvmap.dds"
    
    model <- fmap indexVBO . loadObj $ "asset/room_thickwalls.obj"
    
    buffers <- sequence . map (bufferData GL_ARRAY_BUFFER) $ init model

    let indices = map (map round) (last model) :: [[GLushort]]
    (_,elemBuffer) <- bufferData GL_ELEMENT_ARRAY_BUFFER indices

    frameBuffer <- alloca $ (>>) . glGenFramebuffers 1 <*> peek
    glBindFramebuffer GL_FRAMEBUFFER frameBuffer

    rendertex <- alloca $ (>>) . glGenTextures 1 <*> peek
    glBindTexture GL_TEXTURE_2D rendertex
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_DEPTH_COMPONENT16) 1024 1024 0 GL_DEPTH_COMPONENT GL_FLOAT nullPtr

    let linear = fromIntegral GL_LINEAR
        clampedge = fromIntegral GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER linear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER linear 
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S clampedge
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T clampedge
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_COMPARE_FUNC (fromIntegral GL_LEQUAL)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_COMPARE_MODE (fromIntegral GL_COMPARE_R_TO_TEXTURE)

    glFramebufferTexture GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT rendertex 0
    
    glDrawBuffer GL_NONE
    
    status <- glCheckFramebufferStatus GL_FRAMEBUFFER
    when (status /= fromIntegral GL_FRAMEBUFFER_COMPLETE) undefined

    shader <- compileAndLink "glsl/shadowmap.vertex" "glsl/shadowmap.fragment"

    uniforms <- sequence
        . map (flip (withArray0 0) $ glGetUniformLocation shader)
        $ map (map castCharToCChar) ["model","view","projection","depthbias"]
        
    vectors <- sequence
        . map (flip (withArray0 0) $ glGetUniformLocation shader)
        $ map (map castCharToCChar) ["inversedir","lightpos"]

    texlocns <- sequence
        . map (flip (withArray0 0) $ glGetUniformLocation shader)
        $ map (map castCharToCChar) ["sample","shadowmap"]

    let quaddata = [-1,-1,0,1,-1,0,-1,1,0,-1,1,0,1,-1,0,1,1,0] :: [GLfloat]

    quadBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    glBindBuffer GL_ARRAY_BUFFER quadBuffer
    withArray quaddata $ \ptr -> glBufferData GL_ARRAY_BUFFER 72 ptr GL_STATIC_DRAW

    quadshader <- compileAndLink "glsl/passthrough.vertex" "glsl/passthrough.fragment"
    quadtex <- withArray0 0 (map castCharToCChar "texture") $ glGetUniformLocation quadshader

    control <- newControl window

    loop window control
        depthshader depthlocn frameBuffer rendertex
        shader uniforms vectors texlocns texture buffers elemBuffer (fromIntegral . length $ indices)
        quadshader quadBuffer quadtex

    sequence_ $ map (flip with (glDeleteBuffers 1) . snd) buffers

    withArray [texture, rendertex] (glDeleteTextures 2)

    sequence_ $ map glDeleteProgram shader depthshader quadshader

    sequence_ $ zipWith ((. ($ 1)) . with)
        [elemBuffer, quadBuffer, vertexArray, frameBuffer]
        [glDeleteBuffers, glDeleteBuffers, glDeleteVertexArrays, glDeleteFramebuffers]

    --freeHaskellFunPtr debugCallback

    GLFW.destroyWindow window
    GLFW.terminate
    where
    bufferData buftyp xs = do
        buffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
        glBindBuffer buftyp buffer
        withArray (concat xs) $ \ptr ->
            glBufferData buftyp (ecs xs) ptr GL_STATIC_DRAW
        return (length . head $ xs, buffer)

    setUniformMatrix id val = case length val of
        9 -> withArray val $ glUniformMatrix3fv id 1 GL_FALSE
        16 -> withArray val $ glUniformMatrix4fv id 1 GL_FALSE
        _ -> putStrLn "Unexpected matrix type."

    setTexture i location texture = do
        glActiveTexture (GL_TEXTURE0 + fromIntegral i)
        glBindTexture GL_TEXTURE_2D texture
        glUniform1i location i

    setBuffer a (c,b) = do
        glBindBuffer GL_ARRAY_BUFFER b
        glVertexAttribPointer a (fromIntegral c) GL_FLOAT GL_FALSE 0 nullPtr

    -- elements*components*size
    ecs :: Storable a => [[a]] -> CPtrdiff
    ecs = fromIntegral . ((*) . length <*> ((*).length.head <*> sizeOf.head.head))

    loop window control
        depthshader depthlocn frameBuffer rendertex
        shader uniforms vectors texlocns  texture buffers elemBuffer indices
        quadshader quadBuffer quadtex = do
        
        glBindFramebuffer GL_FRAMEBUFFER frameBuffer
        glViewport 0 0 1024 1024

        glEnable GL_CULL_FACE
        glCullFace GL_BACK

        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram depthshader
        
        let inversedir = [0.5,2,2]
            depthMVP = matmult4 (orthographic4 (-10) 10 (-10) 10 (-10) 20) $ view4 inversedir [0,0,0] [0,1,0]
        withArray (concat depthMVP) $ glUniformMatrix4fv depthlocn 1 GL_FALSE

        glEnableVertexAttribArray 0
        setBuffer 0 (head buffers)
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER elemBuffer
        glDrawElements GL_TRIANGLES indices GL_UNSIGNED_SHORT nullPtr
        
        glDisableVertexAttribArray 0
        
        (width,height) <- fmap (bimap fromIntegral fromIntegral) (GLFW.getWindowSize window)
        glBindFramebuffer GL_FRAMEBUFFER 0
        glViewport 0 0 width height

        glEnable GL_CULL_FACE
        glCullFace GL_BACK

        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram shader
        
        control' <- computeMatricesFromInputs control
        
        let depthbias = matmult4 [[0.5,0,0,0],[0,0.5,0,0],[0,0,0.5,0],[0.5,0.5,0.5,1]] depthMVP
        sequence_ $ zipWith setUniformMatrix
            uniforms
            (map concat [identity4, getView control', getProjection control',depthbias])

        (\[x,y,z] -> glUniform3f (head vectors) x y z) inversedir
        
        sequence_ $ zipWith3 setTexture [0..] texlocns [texture,rendertex]

        sequence_ $ map (glEnableVertexAttribArray.fromIntegral) [0..length buffers - 1]
        
        sequence_ $ zipWith setBuffer [0..] buffers
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER $ elemBuffer
        
        glDrawElements GL_TRIANGLES indices GL_UNSIGNED_SHORT nullPtr
        
        sequence_ $ map (glDisableVertexAttribArray.fromIntegral) [0..length buffers - 1]

        glViewport 0 0 256 256

        glUseProgram quadshader

        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D rendertex
        glUniform1i quadtex 0

        glEnableVertexAttribArray 0
        glBindBuffer GL_ARRAY_BUFFER quadBuffer
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

        -- You have to disable GL_COMPARE_R_TO_TEXTURE above in order to see anything !
        glDrawArrays GL_TRIANGLES 0 6
        glDisableVertexAttribArray 0
        
        GLFW.swapBuffers window
        
        GLFW.pollEvents
        
        escape <- GLFW.getKey window GLFW.Key'Escape
        quit <- GLFW.windowShouldClose window
        when (escape /= GLFW.KeyState'Pressed && not quit) $
            loop window control'
                depthshader depthlocn frameBuffer rendertex
                shader uniforms vectors texlocns texture buffers elemBuffer indices
                quadshader quadBuffer quadtex

openWindow title width height = do
    GLFW.defaultWindowHints
    -- Set up PixelFormat in GLFW
    GLFW.windowHint (GLFW.WindowHint'Samples (Just 4))
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
