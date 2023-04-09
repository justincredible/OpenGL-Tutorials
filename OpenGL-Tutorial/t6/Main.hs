import Control.Monad
import Data.Bits
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

import Controls
import Maths
import ShaderCompilinker

main = do
    glfwInit <- Graphics.UI.GLFW.init
    
    when (not glfwInit) $ putStrLn "Failed to initialize GLFW\n"
    
    let width = 800
        height = 600
    (window,(scrWidth, scrHeight)) <- openWindow "Tutorial 6" width height
    
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
    {-let mvp = buildModelViewPerspective
            0.785398163 (fromIntegral width/fromIntegral height) 0.1 100.0
            [4,3,3] [0,0,0] [0,1,0]
            1 1 1 0.785398163 0.785398163 0.785398163 1-}

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

    controls <- defaultControls window (fromIntegral scrWidth) (fromIntegral scrHeight)
    
    loop window controls shader vertexBuffer coordBuffer texture textureID matrixID
    
    with coordBuffer $ glDeleteBuffers 1
    with vertexBuffer $ glDeleteBuffers 1
    with vertexArray $ glDeleteVertexArrays 1
    with texture $ glDeleteTextures 1
    glDeleteProgram shader
    
    destroyWindow window
    terminate
    where
    loop window controls shader vertexBuffer coordBuffer texture textureID matrixID = do
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
        
        glUseProgram shader
        
        controls' <- computeMatricesFromInputs controls
        let mvp = matmult4 (getProjection controls') (getView controls')
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
        when (escape /= KeyState'Pressed && not quit) $ loop window controls' shader vertexBuffer coordBuffer texture textureID matrixID

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
    readWord32 = BS.foldr (\a b -> b*2^finiteBitSize a + fromIntegral a) 0
    formatValues fourCC
        | fourCC == (BS.pack . map c2w) "DXT1" = (3,GL_COMPRESSED_RGB_S3TC_DXT1_EXT,8)
        | fourCC == (BS.pack . map c2w) "DXT3" = (4,GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,16)
        | fourCC == (BS.pack . map c2w) "DXT5" = (4,GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,16)
        | otherwise = (4,0,16)
    loadMipMaps (mmcount, format, blksz, ptr, width, height, level) = when (mmcount /= level) $ do
        let size = (quot (width+3) 4)*(quot (height+3) 4)*blksz
        glCompressedTexImage2D GL_TEXTURE_2D level format width height 0 size ptr
        loadMipMaps (mmcount, format, blksz, plusPtr ptr $ fromIntegral size, quot width 2, quot height 2, level + 1)
    
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
    1,-1,-1,
    1,-1,1,
    1,-1,1,
    -1,-1,1,
    -1,-1,-1,
    
    -1,-1,-1,
    -1,1,-1,
    1,1,-1,
    1,1,-1,
    1,-1,-1,
    -1,-1,-1,
    
    -1,-1,-1,
    -1,-1,1,
    -1,1,1,
    -1,1,1,
    -1,1,-1,
    -1,-1,-1,

    1,1,-1,
    1,1,1,
    1,-1,1,
    1,-1,1,
    1,-1,-1,
    1,1,-1,

    1,-1,1,
    1,1,1,
    -1,1,1,
    -1,1,1,
    -1,-1,1,
    1,-1,1,

    -1,1,1,
    1,1,1,
    1,1,-1,
    1,1,-1,
    -1,1,-1,
    -1,1,1 ] :: [GLfloat]
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
