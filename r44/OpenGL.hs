module OpenGL(OpenGL(..)
    , initialize
    , beginScene
    , identityLH
    , turnZBufferOn
    , turnZBufferOff
    , setBackBufferRenderTarget
    , turnOnAlphaBlending
    , turnOffAlphaBlending) where

import Data.Foldable
import Foreign.Ptr
import Graphics.UI.GLFW
import Graphics.GL
import Graphics.Win32.GDI.Types

import Maths

data OpenGL = OpenGL {
    getOrthographic :: [GLfloat],
    getHalfOrtho :: [GLfloat],
    getProjection :: [GLfloat] }
    deriving (Eq, Show)

initialize window width height near far = do
    glEnable GL_DEPTH_TEST -- default less
    glEnable GL_CULL_FACE -- default back
    
    swapInterval 1
    
    glViewport 0 0 (fromIntegral width) (fromIntegral height)
    
    return $ OpenGL
        (orthoGraphicLH (fromIntegral width) (fromIntegral height) near far)
        (orthoGraphicLH (fromIntegral $ quot width 2) (fromIntegral $ quot height 2) (near/10) far)
        (perspectiveFovLH (pi/4) (fromIntegral width/fromIntegral height) near far)

beginScene red green blue alpha = do
    glClearColor red green blue alpha
    glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)

turnZBufferOff :: IO ()
turnZBufferOff = glDisable GL_DEPTH_TEST
turnZBufferOn :: IO ()
turnZBufferOn = glEnable GL_DEPTH_TEST

setBackBufferRenderTarget width height = do
    glBindFramebuffer GL_FRAMEBUFFER 0
    glViewport 0 0 width height

turnOnAlphaBlending :: IO ()
turnOnAlphaBlending = glEnable GL_BLEND
turnOffAlphaBlending :: IO ()
turnOffAlphaBlending = glDisable GL_BLEND
