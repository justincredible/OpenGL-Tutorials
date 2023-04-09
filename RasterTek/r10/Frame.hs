module Frame (Frame,Frame.initialize,render,shutdown) where

import Graphics.UI.GLFW

import Camera
import Flow.Render
import Flow.Shutdown
import Light
import LightShader
import Maths
import Model
import OpenGL

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getDemoRotation :: Float,
    getCamera :: Camera,
    getLight :: Light,
    getModel :: Maybe Model,
    getTextureShader :: Maybe LightShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- Camera.initialize
    light <- Light.initialize
    (success, model) <- Model.initialize "asset/cube.txt" "asset/seafloor.tga" 0 True
    if not success
    then return (False, Nothing)
    else do
        (success, shader) <- LightShader.initialize
        return (success, Just $ Frame window opengl 0 camera light model shader)

instance Render Frame where
    render frame@(Frame window opengl rotation camera light (Just model) (Just shader)) = do
        beginScene 0 0 0 1
        
        camera' <- fmap snd . render $ camera
        
        parameters shader
            (yRotationLH rotation)
            (getView camera')
            (getProjection opengl)
            (getPosition camera')
            (getTextureUnit model)
            (getDirection light)
            (getAmbient light)
            (getDiffuse light)
            (getSpecular light)
            (getPower light)

        render model

        endScene window

        return . (,) True $ frame {
            getCamera = camera',
            getDemoRotation = if rotation + 0.02 > 6.2831853 then 0 else rotation + 0.02 }

instance Shutdown Frame where
    shutdown (Frame _ _ _ _ _ model shader) = shutdown shader >> shutdown model
