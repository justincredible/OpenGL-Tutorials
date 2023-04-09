module Frame (Frame,Frame.initialize,Frame.render) where

import Graphics.Rendering.OpenGL as GL hiding (Shader)
import Graphics.UI.GLFW as GLFW

import Camera
import Model
import Shader
import OpenGL

data Frame = Frame Window (GLmatrix GLfloat) (GLmatrix GLfloat) Camera Model Shader

initialize window width height = do
    identity <- identityLH
    projection <- perspectiveFovLH 0.785398163 (fromIntegral width/fromIntegral height) 0.1 1000
    camera <- Camera.initialize
    model <- Model.initialize
    shader <- Shader.initialize
    return $ Frame window identity projection camera model shader

render (Frame window identity projection camera model shader) = do
    GL.clearColor $= Color4 0 0 0 1
    GL.clear [ColorBuffer, DepthBuffer]
    
    view <- Camera.render camera
    
    Shader.render shader identity view projection
    
    Model.render model
    
    GLFW.swapBuffers window
