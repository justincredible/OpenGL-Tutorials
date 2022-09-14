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
import Rotor
import ShaderCompilinker
import Texture

main = do
    glfwInit <- GLFW.init

    when (not glfwInit) $ putStrLn "Failed to initialize GLFW\n"

    let width = 800
        height = 600
    (window,(scrWidth, scrHeight)) <- openWindow "Tutorial 8" width height

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
    glCullFace GL_BACK

    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
    glBindVertexArray vertexArray

    shader <- compileAndLink "glsl/billboard.vertex" "glsl/billboard.fragment"

    uniforms <- sequence
        . map (flip (withArray0 0) $ glGetUniformLocation shader)
        $ map (map castCharToCChar) ["cameraright","cameraup","bbpos","bbsize"]
        
    viewproj <- withArray0 0 (map castCharToCChar "viewproj") $ glGetUniformLocation shader

    texlocn <- withArray0 0 (map castCharToCChar "sample") $ glGetUniformLocation shader
        
    lifelvl <- withArray0 0 (map castCharToCChar "lifelvl") $ glGetUniformLocation shader

    texture <- loadDDS "asset/ExampleBillboard.dds"

    let bbverts = [ -0.5,-0.5,0, 0.5,-0.5,0, -0.5,0.5,0, 0.5,0.5,0 ] :: [GLfloat]
        bufsize = fromIntegral $ sizeOf (head bbverts)*length bbverts
    bbbuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    glBindBuffer GL_ARRAY_BUFFER bbbuffer
    withArray bbverts $ \ptr->
        glBufferData GL_ARRAY_BUFFER bufsize ptr GL_DYNAMIC_DRAW

    cubeshader <- compileAndLink "glsl/color.vertex" "glsl/color.fragment"
    mvp <- withArray0 0 (map castCharToCChar "MVP") $ glGetUniformLocation cubeshader
    [p,_,n] <- loadObj $ "asset/cube.obj"
    [cvbuffer, ccbuffer] <- sequence $
        map (bufferData GL_ARRAY_BUFFER) [p,map (\xs -> if (< 0) . sum $ xs then add [1,1,1] xs else xs) n]
    
    control <- newControl window scrWidth scrHeight

    loop window control
        shader uniforms viewproj texlocn texture bbbuffer lifelvl
        cubeshader mvp [cvbuffer, ccbuffer] 0

    with texture $ glDeleteTextures 1

    glDeleteProgram shader
    glDeleteProgram cubeshader

    sequence_ $ zipWith ((. ($ 1)) . with)
        [bbbuffer, snd cvbuffer, snd ccbuffer, vertexArray]
        [glDeleteBuffers, glDeleteBuffers, glDeleteBuffers, glDeleteVertexArrays]

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

    setUniformMatrix id val = withArray val $ glUniformMatrix4fv id 1 GL_FALSE

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
        shader uniforms viewproj texlocn texture bbbuffer lifelvl
        cubeshader mvp buffers rotation = do
        
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)

        Just time <- GLFW.getTime
        let deltatime = time - getLastTime control

        control' <- computeMatricesFromInputs control
        let view = getView control'
            projection = getProjection control'
            camerapos = currentPosition control'
            
        glDisable GL_BLEND
        glUseProgram cubeshader
        let rotation' = rotation + realToFrac deltatime
            model = matmult4 (scale4 0.2 0.2 0.2) (rotateY4 rotation')
        setUniformMatrix mvp . concat $ matmult4 projection (matmult4 view model)
        glEnableVertexAttribArray 0 >> glEnableVertexAttribArray 1
        sequence_ $ zipWith setBuffer [0..] buffers
        glDrawArrays GL_TRIANGLES 0 36
        glDisableVertexAttribArray 0 >> glDisableVertexAttribArray 1

        glEnable GL_BLEND
        glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

        glUseProgram shader

        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D texture
        glUniform1i texlocn 0

        let [cameraright,cameraup,bbpos,bbsize] = uniforms
        glUniform3f cameraright (view!!0!!0) (view!!1!!0) (view!!2!!0)
        glUniform3f cameraup (view!!0!!1) (view!!1!!1) (view!!2!!0)

        glUniform3f bbpos 0 0.5 0
        glUniform2f bbsize 1 0.125

        glUniform1f lifelvl $ sin (realToFrac time)*0.1 + 0.7

        setUniformMatrix viewproj . concat $ matmult4 projection view

        glEnableVertexAttribArray 0
        glBindBuffer GL_ARRAY_BUFFER bbbuffer
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

        glDrawArrays GL_TRIANGLE_STRIP 0 4

        glDisableVertexAttribArray 0

        GLFW.swapBuffers window

        GLFW.pollEvents
        
        escape <- GLFW.getKey window GLFW.Key'Escape
        quit <- GLFW.windowShouldClose window
        when (escape /= GLFW.KeyState'Pressed && not quit) $
            loop window control' { getLastTime = time }
                shader uniforms viewproj texlocn texture bbbuffer lifelvl
                cubeshader mvp buffers rotation'

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
