module Frame (render) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

render win = do
    GL.clearColor $= Color4 0 0 0 1
    GL.clear [ColorBuffer, DepthBuffer]
    
    GLFW.swapBuffers win
