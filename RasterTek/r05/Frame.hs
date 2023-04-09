module Frame (Frame,Frame.initialize,render,shutdown) where

import Graphics.UI.GLFW

import Camera
import Flow.Render
import Flow.Shutdown
import Maths
import Model
import OpenGL
import TextureShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getRotation :: Float,
    getCamera :: Camera,
    getModel :: Maybe Model,
    getTextureShader :: Maybe TextureShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- Camera.initialize
    (success, model) <- Model.initialize "asset/test.tga" 0 True
    if not success
    then return (False, Nothing)
    else do
        (success, shader) <- TextureShader.initialize
        return (success, Just $ Frame window opengl 0 camera model shader)

instance Render Frame where
    render frame@(Frame window opengl rotation camera (Just model) (Just shader)) = do
        beginScene 0 0 0 1
        
        camera' <- fmap snd . render $ camera
        
        parameters shader
            (yRotationLH rotation)
            (getView camera')
            (getProjection opengl)
            (getTextureUnit model)
        
        render model
        
        endScene window
        
        return . (,) True $ frame {
            getCamera = camera',
            getRotation = if rotation + 0.02 > 6.2831853 then 0 else rotation + 0.02 }

instance Shutdown Frame where
    shutdown (Frame _ _ _ _ model shader) = shutdown shader >> shutdown model
