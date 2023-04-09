import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.List hiding (transpose)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import Linear.Matrix
import Linear.Metric
import Linear.V3
import Linear.V4
import Linear.Vector
import Linear.Quaternion
import System.Exit
import System.Random
import System.Win32.Info
import System.Win32.Info.Computer

import Control
import qualified Maths as Maths
import Object
import Rotor
import ShaderCompilinker
import Texture

maxParticles = 10^5

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

    glEnable GL_DEPTH_TEST
    glEnable GL_CULL_FACE

    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
    glBindVertexArray vertexArray

    shader <- compileAndLink "glsl/standard.vertex" "glsl/standard.fragment"

    uniforms <- sequence
        . map (flip (withArray0 0) $ glGetUniformLocation shader)
        $ map (map castCharToCChar) ["projection","view","model"]

    texlocn <- withArray0 0 (map castCharToCChar "sample") $ glGetUniformLocation shader
    
    lightpos <- withArray0 0 (map castCharToCChar "lightpos") $ glGetUniformLocation shader

    texture <- loadDDS "asset/uvsuzanne.dds"

    model <- fmap indexVBO . loadObj $ "asset/suzanne.obj"
    buffers <- sequence $ map (bufferData GL_ARRAY_BUFFER) (init model)

    let indices = map (map round) (last model) :: [[GLushort]]
    (_,elemBuffer) <- bufferData GL_ELEMENT_ARRAY_BUFFER indices
    
    control <- newControl window scrWidth scrHeight
    
    Just time <- GLFW.getTime
    
    positions <- sequence $ replicate 100 randomPosition
    orientations <- sequence $ replicate 100 randomOrientation
    
    loop window control
        shader uniforms texlocn texture buffers elemBuffer (fromIntegral . length $ indices)
        lightpos time 0
        positions orientations False

    with texture $ glDeleteTextures 1

    glDeleteProgram shader

    sequence_ $ map (flip with (glDeleteBuffers 1).snd) buffers
    sequence_ $ zipWith ((. ($ 1)) . with)
        [elemBuffer, vertexArray]
        [glDeleteBuffers, glDeleteVertexArrays]

    --freeHaskellFunPtr debugCallback

    GLFW.destroyWindow window
    GLFW.terminate
    where
    randomPosition :: IO (V3 GLfloat)
    randomPosition = do
        x <- randomRIO (-10,9)
        y <- randomRIO (-10,9)
        z <- randomRIO (-10,9)
        return $ V3 x y z
    randomOrientation :: IO (Quaternion GLfloat)
    randomOrientation = do
        x <- randomRIO (0,359)
        y <- randomRIO (0,359)
        z <- randomRIO (0,359)
        s <- randomRIO (0,pi)
        return . normalize $ axisAngle (V3 x y z) s

    bufferData buftyp xs = do
        buffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
        glBindBuffer buftyp buffer
        withArray (concat xs) $ \ptr ->
            glBufferData buftyp (ecs xs) ptr GL_STATIC_DRAW
        return (length . head $ xs, buffer)

    ecs :: Storable a => [[a]] -> CPtrdiff
    ecs = fromIntegral . ((*) . length <*> ((*).length.head <*> sizeOf.head.head))

    setUniformMatrix id val = withArray val $ glUniformMatrix4fv id 1 GL_FALSE

    setTexture i location texture = do
        glActiveTexture (GL_TEXTURE0 + fromIntegral i)
        glBindTexture GL_TEXTURE_2D texture
        glUniform1i location i

    loop window control
        shader uniforms texlocn texture buffers ibuffer idxcnt
        lightpos lastime framecnt
        positions orientations wasPressed = do

        Just time <- GLFW.getTime
        let framecnt' = framecnt + 1

        control' <- computeMatricesFromInputs control
        let view = linmat . getView $ control'
            projection = linmat . getProjection $ control'

        mouse <- GLFW.getMouseButton window GLFW.MouseButton'1
        when (wasPressed && mouse == GLFW.MouseButtonState'Released) $ do
            let (origin,direction) = screenPosToWorldRay 512 384 1024 768 view projection
            testModel origin direction 0 orientations positions
        let wasPressed' = mouse == GLFW.MouseButtonState'Pressed
        
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)

        glUseProgram shader
        
        glUniform3f lightpos 4 4 4

        setTexture 0 texlocn texture
        
        glBindBuffer GL_ARRAY_BUFFER (snd . head $ buffers)
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

        glBindBuffer GL_ARRAY_BUFFER (snd . head . tail $ buffers)
        glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 0 nullPtr

        glBindBuffer GL_ARRAY_BUFFER (snd . last $ buffers)
        glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE 0 nullPtr
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibuffer

        sequence_ $ map glEnableVertexAttribArray [0..2]
        
        sequence_ $ zipWith (render uniforms idxcnt view projection) orientations positions

        sequence_ $ map glDisableVertexAttribArray [0..2]

        GLFW.swapBuffers window
        GLFW.pollEvents
        
        escape <- GLFW.getKey window GLFW.Key'Escape
        quit <- GLFW.windowShouldClose window
        when (escape /= GLFW.KeyState'Pressed && not quit) $
            loop window control' { getLastTime = time }
                shader uniforms texlocn texture buffers ibuffer idxcnt
                lightpos time framecnt'
                positions orientations wasPressed'

    testModel _ _ i [] [] = return ()
    testModel origin direction i (o:os) (p:ps) = let
        aabbmin = V3 (-1) (-1) (-1)
        aabbmax = V3 1 1 1
        model = mkTransformation o p
        (success,distance) = testRayOBBIntersection origin direction aabbmin aabbmax (transpose model)
        in
        if not $ success
        then testModel origin direction (i+1) os ps
        else putStrLn $ "mesh: " ++ show i ++ "\ndistance: " ++ show distance
    
    render uniforms idxcnt view projection orientation position = do
        let model = mkTransformation orientation zero !*! mkTransformation (Quaternion 1 zero) position
        
        sequence_ $ zipWith setUniformMatrix uniforms (map (concat . map toList . toList . transpose) [projection,view,model])

        glDrawElements GL_TRIANGLES idxcnt GL_UNSIGNED_SHORT nullPtr

    linmat [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]] = V4
        (V4 a e i m)
        (V4 b f j n)
        (V4 c g k o)
        (V4 d h l p)
        {-(V4 a b c d)
        (V4 e f g h)
        (V4 i j k l)
        (V4 m n o p)-}

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

screenPosToWorldRay mousex mousey screenw screenh view projection = let
    lRayStart_NDC = V4
        ((fromIntegral mousex/fromIntegral screenw - 0.5)*2)
        ((fromIntegral mousey/fromIntegral screenh - 0.5)*2)
        (-1) 1 
    lRayEnd_NDC = V4
        ((fromIntegral mousex/fromIntegral screenw - 0.5)*2)
        ((fromIntegral mousey/fromIntegral screenh - 0.5)*2)
        0 1

    invproj = inv44 projection
    inview = inv44 view
    
    lRayStart_camera = (^/) <*> (^._w) $ invproj !* lRayStart_NDC
    lRayStart_world = (^/) <*> (^._w) $ inview !* lRayStart_camera
    lRayEnd_camera = (^/) <*> (^._w) $ invproj !* lRayEnd_NDC
    lRayEnd_world = (^/) <*> (^._w) $ inview !* lRayEnd_camera
    
    lRayDir_world = normalize $ (lRayEnd_world - lRayStart_world) ^._xyz
    in
    (lRayStart_world ^._xyz, lRayDir_world)

testRayOBBIntersection origin direction aabbmin aabbmax mdlmx = let
    obbposition_worldspace = (^._xyz) . last . toList $ mdlmx
    delta = obbposition_worldspace - origin :: V3 Float
    -- Test intersection with the 2 planes perpendicular to the OBB's X axis
    xaxis = (^._xyz) . head . toList $ mdlmx
    yaxis = (^._xyz) . head . tail . toList $ mdlmx
    zaxis = (^._xyz) . head . tail . tail . toList  $ mdlmx
    ex = dot xaxis delta
    fx = dot direction xaxis
    ey = dot yaxis delta
    fy = dot direction yaxis
    ez = dot zaxis delta
    fz = dot direction zaxis
    tx1 = min ((ex + aabbmin ^._x)/fx) ((ex + aabbmax ^._x)/fx)
    tx2 = max ((ex + aabbmin ^._x)/fx) ((ex + aabbmax ^._x)/fx)
    ty1 = min ((ey + aabbmin ^._y)/fy) ((ey + aabbmax ^._y)/fy)
    ty2 = max ((ey + aabbmin ^._y)/fy) ((ey + aabbmax ^._y)/fy)
    tz1 = min ((ez + aabbmin ^._z)/fz) ((ez + aabbmax ^._z)/fz)
    tz2 = max ((ez + aabbmin ^._z)/fz) ((ez + aabbmax ^._z)/fz)
    tmin = maximum [0,
        if abs fx > 0.001 then tx1 else 0,
        if abs fy > 0.001 then ty1 else 0,
        if abs fz > 0.001 then tz1 else 0 ]
    tmax = let m = 10^^5 in minimum [m,
        if abs fx > 0.001 then tx2 else m,
        if abs fy > 0.001 then ty2 else m,
        if abs fz > 0.001 then tz2 else m ]
    --tmin = maximum [0,tx1,ty1,tz1]
    --tmax = minimum [100000,tx2,ty2,tz2]
    in
    if (abs fx > 0.001 || abs fy > 0.001 || abs fz > 0.001) && tmin > tmax
        || abs fx <= 0.001 && (aabbmin ^._x - ex > 0 || aabbmax ^._x - ex < 0)
        || abs fy <= 0.001 && (aabbmin ^._y - ey > 0 || aabbmax ^._y - ey < 0)
        || abs fz <= 0.001 && (aabbmin ^._z - ez > 0 || aabbmax ^._z - ez < 0)
    then (False,-1)
    else (True, maximum [tx1,ty1,tz1])

truncate2DPs dps dbl = case break (== '.') . show $ dbl of
    (prefix,[]) -> prefix  ++ " sec"
    (prefix,rest) -> prefix ++ take dps rest ++ " sec"
{-
triangle = [ 
    -1,-1,0,
    1,-1,0,
    0,1,0 ] :: [GLfloat]
triclr = [
    1,0,0,
    1,0,0,
    1,0,0 ] :: [GLfloat]
-}
{-    
numVert = 36
numClr = numVert
numCrd = numVert
vertices = [
    -1,-1,1,
    -1,-1,-1,
    1,-1,-1,
    1,-1,-1,
    1,-1,1,
    -1,-1,1,
    
    1,-1,-1,
    -1,-1,-1,
    -1,1,-1,
    -1,1,-1,
    1,1,-1,
    1,-1,-1,
    
    -1,1,-1,
    -1,-1,-1,
    -1,-1,1,
    -1,-1,1,
    -1,1,1,
    -1,1,-1,

    1,1,1,
    1,-1,1,
    1,-1,-1,
    1,-1,-1,
    1,1,-1,
    1,1,1,

    1,1,1,
    -1,1,1,
    -1,-1,1,
    -1,-1,1,
    1,-1,1,
    1,1,1,

    1,1,1,
    1,1,-1,
    -1,1,-1,
    -1,1,-1,
    -1,1,1,
    1,1,1 ] :: [GLfloat]
colours = [
    0,0,0,
    1,0,0,
    1,0,1,
    1,0,1,
    0,0,1,
    0,0,0,
    
    0,0,0,
    0,1,0,
    1,1,0,
    1,1,0,
    1,0,0,
    0,0,0,
    
    0,0,0,
    0,0,1,
    0,1,1,
    0,1,1,
    0,1,0,
    0,0,0,

    1,1,0,
    1,1,1,
    1,0,1,
    1,0,1,
    1,0,0,
    1,1,0,

    1,0,1,
    1,1,1,
    0,1,1,
    0,1,1,
    0,0,1,
    1,0,1,

    0,1,1,
    1,1,1,
    1,1,0,
    1,1,0,
    0,1,0,
    0,1,1 ] :: [GLfloat]
texcoords = [
    1/3,0,
    0,0,
    0,1/3,
    0,1/3,
    1/3,1/3,
    1/3,0,

    2/3,0,
    1/3,0,
    1/3,1/3,
    1/3,1/3,
    2/3,1/3,
    2/3,0,

    1,0,
    2/3,0,
    2/3,1/3,
    2/3,1/3,
    1,1/3,
    1,0,
    
    1/3,1/3,
    0,1/3,
    0,2/3,
    0,2/3,
    1/3,2/3,
    1/3,1/3,
    
    2/3,1/3,
    1/3,1/3,
    1/3,2/3,
    1/3,2/3,
    2/3,2/3,
    2/3,1/3,

    1,1/3,
    2/3,1/3,
    2/3,2/3,
    2/3,2/3,
    1,2/3,
    1,1/3 ] :: [GLfloat]
-}