module OpenGL(OpenGL(..)
    , initialize
    , beginScene
    , identityLH
    , turnZBufferOn
    , turnZBufferOff
    , setBackBufferRenderTarget) where

import Data.Foldable
import Foreign.Ptr
import Graphics.UI.GLFW
import Graphics.GL
import Graphics.Win32.GDI.Types

import Maths

data OpenGL = OpenGL {
    getOrthographic :: [GLfloat],
    getProjection :: [GLfloat] }
    deriving (Eq, Show)

initialize window width height = do
    glEnable GL_DEPTH_TEST -- default less
    glEnable GL_CULL_FACE -- default back
    
    swapInterval 1
    
    glViewport 0 0 (fromIntegral width) (fromIntegral height)
    
    return $ OpenGL
        (orthographicLH (fromIntegral width) (fromIntegral height) 0.1 1000)
        (perspectiveFovLH (pi/4) (fromIntegral width/fromIntegral height) 0.1 1000)

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