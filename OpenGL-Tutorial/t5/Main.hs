import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import qualified Data.ByteString.Unsafe   as BSU
import Data.Word
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
    window <- openWindow "Tutorial 5" width height
    
    glClearColor 0 0 0.4 0
    
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    
    vertexArray <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
    glBindVertexArray vertexArray

    shader <- compileAndLink "glsl/Simple.vertexshader" "glsl/Simple.fragmentshader"
    when (shader == 0) $ putStrLn "shader error"

    matrixID <- withArray0 0 (map castCharToCChar "MVP") (glGetUniformLocation shader)
    let mvp = buildModelViewPerspective
            0.785398163 (fromIntegral width/fromIntegral height) 0.1 100.0
            [4,3,3] [0,0,0] [0,1,0]
            1 1 1 0.785398163 0.785398163 0.785398163 1

    texture <- loadDDS "asset/uvtemplate.dds"
    
    textureID <- withArray0 0 (map castCharToCChar "sample") (glGetUniformLocation shader)
    
    let numVert = 36
        numCrd = numVert

    vertexBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    let vertSize = 3*sizeOf (head vertices)
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVert*vertSize) ptr GL_STATIC_DRAW
    
    coordBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ARRAY_BUFFER coordBuffer
    let texSize = 2*sizeOf (head texcoords)
    withArray texcoords $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numCrd*texSize) ptr GL_STATIC_DRAW
    
    loop window shader vertexBuffer coordBuffer texture textureID matrixID mvp
    
    with coordBuffer $ \ptr ->
        glDeleteBuffers 1 ptr
    with vertexBuffer $ \ptr ->
        glDeleteBuffers 1 ptr
    with vertexArray $ \ptr ->
        glDeleteVertexArrays 1 ptr    
    glDeleteProgram shader
    with texture $ glDeleteTextures 1
    
    destroyWindow window
    terminate
    where
    loop window shader vertexBuffer coordBuffer texture textureID matrixID mvp = do
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram shader

        withArray (concat mvp) $ glUniformMatrix4fv matrixID 1 GL_FALSE
        
        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D texture
        glUniform1i textureID 0

        glEnableVertexAttribArray 0
        glEnableVertexAttribArray 1
        
        glBindBuffer GL_ARRAY_BUFFER vertexBuffer
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
        
        glBindBuffer GL_ARRAY_BUFFER coordBuffer
        glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 0 nullPtr
        
        glDrawArrays GL_TRIANGLES 0 36
        
        glDisableVertexAttribArray 1
        glDisableVertexAttribArray 0
        
        swapBuffers window
        
        pollEvents
        
        escape <- getKey window Key'Escape
        quit <- windowShouldClose window
        when (escape /= KeyState'Pressed && not quit) $ loop window shader vertexBuffer coordBuffer texture textureID matrixID mvp

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

loadDDS file = do
    (fileType,(header,compressed)) <- fmap (fmap (BS.splitAt 124).BS.splitAt 4) $
        BS.readFile file
    
    if fileType /= (BS.pack . map c2w) "DDS " || BS.length header /= 124
    then return 0  
    else do
        let height = readWord32 . BS.take 4 . BS.drop 8 $ header
            width = readWord32 . BS.take 4 . BS.drop 12 $ header
            linearSize = readWord32 . BS.take 4 . BS.drop 16 $ header
            mipMapCount = readWord32 . BS.take 4 . BS.drop 24 $ header
            fourCC = BS.take 4 . BS.drop 80 $ header
            bufsize = (if mipMapCount > 1 then (*2) else id) linearSize
            buffer = BS.take (fromIntegral bufsize) compressed
            (components,format,blocksize) = formatValues fourCC
            offset = 0
        
        textureID <- alloca $ \ptr -> do
            glGenTextures 1 ptr
            peek ptr
        
        glBindTexture GL_TEXTURE_2D textureID
        
        withArray (BS.unpack compressed) $ \ptr ->
            loadMipMaps (fromIntegral mipMapCount,format,blocksize,ptr,fromIntegral width,fromIntegral height,0)
            
        return textureID

    where
    readWord32 :: ByteString -> Word32
    readWord32 = BS.foldr (\a b -> b*256 + fromIntegral a) 0
    formatValues fourCC
        | fourCC == (BS.pack . map c2w) "DXT1" = (3,GL_COMPRESSED_RGB_S3TC_DXT1_EXT,8)
        | fourCC == (BS.pack . map c2w) "DXT3" = (4,GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,16)
        | fourCC == (BS.pack . map c2w) "DXT5" = (4,GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,16)
        | otherwise = (4,0,16)
    loadMipMaps (mmcount, format, blksz, ptr, width, height, level) = when (mmcount /= level) $ do
        let size = (quot (width+3) 4)*(quot (height+3) 4)*blksz
        glCompressedTexImage2D GL_TEXTURE_2D level format width height 0 size ptr
        loadMipMaps (mmcount, format, blksz, plusPtr ptr (fromIntegral size), quot width 2, quot height 2, level + 1)
    
triangle = [ 
    -1,-1,0,
    1,-1,0,
    0,1,0 ] :: [GLfloat]
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
texcoords = [
    0.000059,0.000004,
    0.000103,0.336048,
    0.335973,0.335903,
    1.000023,0.000013,
    0.667979,0.335851,
    0.999958,0.336064,
    0.667979,0.335851,
    0.336024,0.671877,
    0.667969,0.671889,
    1.000023,0.000013,
    0.668104,0.000013,
    0.667979,0.335851,
    0.000059,0.000004,
    0.335973,0.335903,
    0.336098,0.000071,
    0.667979,0.335851,
    0.335973,0.335903,
    0.336024,0.671877,
    1.000004,0.671847,
    0.999958,0.336064,
    0.667979,0.335851,
    0.668104,0.000013,
    0.335973,0.335903,
    0.667979,0.335851,
    0.335973,0.335903,
    0.668104,0.000013,
    0.336098,0.000071,
    0.000103,0.336048,
    0.000004,0.671870,
    0.336024,0.671877,
    0.000103,0.336048,
    0.336024,0.671877,
    0.335973,0.335903,
    0.667969,0.671889,
    1.000004,0.671847,
    0.667979,0.335851 ] :: [GLfloat]
