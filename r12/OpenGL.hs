module OpenGL(OpenGL,initialize,beginScene,endScene, getOrthographic, getProjection, identityLH, translationLH, yRotationLH,orthoGraphicLH,turnZBufferOn,turnZBufferOff) where

import Data.Foldable
import Foreign.Ptr
import Graphics.UI.GLFW
import Graphics.GL
import Graphics.Win32.GDI.Types

data OpenGL = OpenGL { getOrthographic :: [GLfloat], getProjection :: [GLfloat] }
    deriving (Eq, Show)

initialize window width height = do
    glEnable GL_DEPTH_TEST -- default less
    glEnable GL_CULL_FACE -- default back
    
    swapInterval 1
    
    glViewport 0 0 (fromIntegral width) (fromIntegral height)
    
    return $ OpenGL
        (orthoGraphicLH (fromIntegral width) (fromIntegral height) 0.1 1000)
        (perspectiveFovLH 0.785398163 (fromIntegral width/fromIntegral height) 0.1 1000)

identityLH :: [GLfloat]
identityLH = take 16 . cycle $ [1,0,0,0,0]

perspectiveFovLH fieldOfView aspect near depth = let
    rtfov = recip . tan . (*0.5) $ fieldOfView
    denom = depth - near
    in
    [ rtfov*recip aspect, 0, 0, 0
    , 0, rtfov, 0, 0
    , 0, 0, (depth+near)/denom, 1
    , 0, 0, negate $ depth*near/denom, 0 ]

yRotationLH angle =
    [ cos angle, 0, -sin angle, 0
    , 0, 1, 0, 0
    , sin angle, 0, cos angle, 0
    , 0, 0, 0, 1 ]

translationLH x y z =
    [ 1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , x, y, z, 1 ]

orthoGraphicLH width height near far =
    [ 2/width, 0, 0, 0
    , 0, 2/height, 0, 0
    , 0, 0, 2/(near-far), 0
    , 0, 0, (far + near)/(near-far), 1 ]

beginScene red green blue alpha = do
    glClearColor red green blue alpha
    glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)

endScene window = do
    swapBuffers window

turnZBufferOff :: IO ()
turnZBufferOff = glDisable GL_DEPTH_TEST
turnZBufferOn :: IO ()
turnZBufferOn = glEnable GL_DEPTH_TEST
