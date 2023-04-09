module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Camera
import DeferredBuffers
import DeferredShader
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Maths
import Light
import Model
import OpenGL
import OrthoWindow
import PostLightShader

nearPlane = 0.1
farPlane = 1000

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getDemoRot :: Float,
    getCamera :: Camera,
    getLight :: Light,
    getModel :: Maybe Model,
    getFullScreen :: Maybe OrthoWindow,
    getRenderTexas :: Maybe DeferredBuffers,
    getDeferredShader :: Maybe DeferredShader,
    getLightShader :: Maybe PostLightShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render
    light <- Light.initialize
    
    (mdlscs,model) <- Model.initialize "asset/cube.txt" "asset/seafloor.tga" 1 True
    (fullscs,full) <- OrthoWindow.initialize width height
    (bufscs,buffers) <- DeferredBuffers.initialize width height 2 3
    
    (dscs,dshader) <- DeferredShader.initialize
    (lscs,lshader) <- PostLightShader.initialize
    
    return (and [mdlscs,fullscs,bufscs,dscs,lscs], Just $
        Frame window opengl 0 camera { getBaseView = getView camera }
            light model full buffers dshader lshader)

instance Update Frame where
    update frame None = let rotation = getDemoRot frame + 0.01 in
        return (True, frame {
            getDemoRot = if rotation > 2*pi then rotation - 2*pi else rotation })
            
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Render Frame where
    render frame = do
        render (Sub ToTexture frame)
        
        render (Sub Scene frame)
        
        swapBuffers . getWindow $ frame
        
        return (True, frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getFullScreen $ frame
        shutdown . getRenderTexas $ frame
        shutdown . getDeferredShader $ frame
        shutdown . getLightShader $ frame

data RenderType = Scene | ToTexture

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub ToTexture frame) = do
        render (getRenderTexas frame)
        
        beginScene 0 0 0 1
        
        let Just model = getModel frame
            Just shader = getDeferredShader frame

        DeferredShader.parameters shader
            (yRotationLH . getDemoRot $ frame)
            (getView . getCamera $ frame)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
        
        render model
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub ToTexture frame)

    render (Sub Scene frame) = do
        beginScene 0 0 0 1
        
        turnZBufferOff
        
        let Just buffers = getRenderTexas frame
            Just shader = getLightShader frame
        
        PostLightShader.parameters shader
            identityLH
            (getBaseView . getCamera $ frame)
            (getOrthographic . getOpenGL $ frame)
            (colorUnit buffers)
            (normalUnit buffers)
            (getDirection . getLight $ frame)
        
        render (getFullScreen frame)
        
        turnZBufferOn
        
        return (True,Sub Scene frame)
