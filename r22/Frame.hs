module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Camera
import DebugWindow
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Light
import LightShader
import Maths
import Model
import OpenGL
import RenderTexture
import TextureShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getDemoRot :: GLfloat,
    getCamera :: Camera,
    getLight :: Light,
    getModel :: Maybe Model,
    getRenderTexture :: Maybe RenderTexture,
    getDebugWindow :: Maybe DebugWindow,
    getLightShader :: Maybe LightShader,
    getTextureShader :: Maybe TextureShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    light <- Light.initialize
    (modelscs,model) <- Model.initialize "asset/cube.txt" "asset/seafloor.tga" 1 False
    (rentexscs,rentex) <- RenderTexture.initialize width height 2
    (dbgwndscs,dbgwnd) <- DebugWindow.initialize width height 100 100
    (lshaderscs,lshader) <- LightShader.initialize
    (tshaderscs,tshader) <- TextureShader.initialize
    return (and [modelscs,rentexscs,dbgwndscs,lshaderscs,tshaderscs], Just $
        Frame window opengl 0 camera light model rentex dbgwnd lshader tshader)

instance Render Frame where
    render frame = do
        (_,camera) <- render (getCamera frame)
        let frame' = frame { getCamera = camera }
        
        render (SubRender ToTexture frame')
        
        beginScene 0 0 0 1
        
        render (SubRender Scene frame')
        
        turnZBufferOff
        
        let Just rentex = getRenderTexture frame
            Just shader = getTextureShader frame
        
        TextureShader.parameters shader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (textureUnit rentex)
        
        let Just dbgwnd = getDebugWindow frame

        update dbgwnd (II 50 50) >>= (render . snd)
        
        turnZBufferOn
        
        swapBuffers . getWindow $ frame
        
        let rotation = (+ pi*0.0025) . getDemoRot $ frame
        return . (,) True $ frame { getCamera = camera, getDemoRot = if rotation > 2*pi then rotation - 2*pi else rotation }

instance Update Frame where
    update frame None = return (True, frame { getCamera = (getCamera frame) { getPosition = [0,0,-5] }})
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getRenderTexture $ frame
        shutdown . getDebugWindow $ frame
        shutdown . getLightShader $ frame
        shutdown . getTextureShader $ frame

data RenderType = Scene | ToTexture

data SubRender = SubRender RenderType Frame

instance Render SubRender where
    render subren@(SubRender ToTexture frame) = do
        let Just rentex = getRenderTexture frame
        setRenderTarget rentex
        
        beginScene 0 0 1 1
        
        render (SubRender Scene frame)
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,subren)
    
    render subren@(SubRender Scene frame) = do
        let Just model = getModel frame
            Just shader = getLightShader frame
            light = getLight frame
        
        LightShader.parameters shader
            (yRotationLH . getDemoRot $ frame)
            (getView . getCamera $ frame)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
            (getDirection . getLight $ frame)
            (getDiffuse . getLight $ frame)
            
        render model
        
        return (True, subren)
