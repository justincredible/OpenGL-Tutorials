module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Camera
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import HorizontalShader
import VerticalShader
import Maths
import Model
import OpenGL
import OrthoWindow
import RenderTexture
import TextureShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getDemoRot :: Float,
    getModel :: Maybe Model,
    getSmallWindow :: Maybe OrthoWindow,
    getFullScreen :: Maybe OrthoWindow,
    getRenderTexture :: Maybe RenderTexture,
    getDownSample :: Maybe RenderTexture,
    getHorizontalTexture :: Maybe RenderTexture,
    getVerticalTexture :: Maybe RenderTexture,
    getUpSample :: Maybe RenderTexture,
    getTextureShader :: Maybe TextureShader,
    getHorizontalShader :: Maybe HorizontalShader,
    getVerticalShader :: Maybe VerticalShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    
    let downWidth = quot width 2
        downHeight = quot height 2

    (modelscs,model) <- Model.initialize "asset/cube.txt" "asset/seafloor.tga" 1 True
    (smallscs,small) <- OrthoWindow.initialize downWidth downHeight
    (fullscs,full) <- OrthoWindow.initialize width height
    (rentexscs,rentex) <- RenderTexture.initialize width height 2
    (downscs,down) <- RenderTexture.initialize downWidth downHeight 3
    (horiscs,hori) <- RenderTexture.initialize downWidth downHeight 4
    (vertscs,vert) <- RenderTexture.initialize downWidth downHeight 5
    (upscs,up) <- RenderTexture.initialize width height 6
    (tshaderscs,tshader) <- TextureShader.initialize
    (hshaderscs,hshader) <- HorizontalShader.initialize
    (vshaderscs,vshader) <- VerticalShader.initialize
    
    return (and [modelscs,smallscs,fullscs,rentexscs,downscs,horiscs,vertscs,upscs,tshaderscs,hshaderscs,vshaderscs], Just $
        Frame window opengl camera 0 model small full rentex down hori vert up tshader hshader vshader)

instance Render Frame where
    render frame = do
        (_,Sub _ frame') <- render (Sub ToTexture frame)
        
        turnZBufferOff
        
        render (Sub DownSample frame')
        
        render (Sub HorizontalBlur frame')
        
        render (Sub VerticalBlur frame')
        
        render (Sub UpSample frame')
        
        render (Sub Scene frame')
        
        turnZBufferOn
        
        swapBuffers . getWindow $ frame'
        
        return (True,frame')

instance Update Frame where
    update frame None = let rotation = getDemoRot frame + 0.005 in
        return (True, frame {
            getCamera = (getCamera frame) { getPosition = [0,0,-10] },
            getDemoRot = if rotation > 2*pi then rotation - 2*pi else rotation })
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getSmallWindow $ frame
        shutdown . getFullScreen $ frame
        shutdown . getRenderTexture $ frame
        shutdown . getDownSample $ frame
        shutdown . getHorizontalTexture $ frame
        shutdown . getVerticalTexture $ frame
        shutdown . getUpSample $ frame
        shutdown . getTextureShader $ frame
        shutdown . getHorizontalShader $ frame
        shutdown . getVerticalShader $ frame

data RenderType = Scene | ToTexture | UpSample | DownSample | HorizontalBlur | VerticalBlur

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub ToTexture frame) = do
        render (getRenderTexture frame)
        
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just model = getModel frame
            Just shader = getTextureShader frame
            
        TextureShader.parameters shader
            (yRotationLH (getDemoRot frame))
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
        
        render model
        
        return (True,Sub ToTexture frame { getCamera = camera })
    
    render (Sub DownSample frame) = do
        render (getDownSample frame)
        
        beginScene 1 0 0 1
        
        let camera = getCamera frame
            Just rentex = getRenderTexture frame
            Just shader = getTextureShader frame
        
        TextureShader.parameters shader
            identityLH
            (getView camera)
            (getHalfOrtho . getOpenGL $ frame)
            (textureUnit rentex)
        
        render (getSmallWindow frame)
        
        return (True,Sub DownSample frame)
    
    render (Sub HorizontalBlur frame) = do
        render (getHorizontalTexture frame)
        
        beginScene 0 1 0 1
        
        let camera = getCamera frame
            Just down = getDownSample frame
            Just shader = getHorizontalShader frame
        
        (width,height) <- getWindowSize (getWindow frame)
        
        HorizontalShader.parameters shader
            identityLH
            (getView camera)
            (getHalfOrtho . getOpenGL $ frame)
            (fromIntegral width)
            (textureUnit down)
        
        render (getSmallWindow frame)
        
        return (True,Sub HorizontalBlur frame)
    
    render (Sub VerticalBlur frame) = do
        render (getVerticalTexture frame)
        
        beginScene 0 0 1 1
        
        let camera = getCamera frame
            Just horiblur = getHorizontalTexture frame
            Just shader = getVerticalShader frame
        
        (width,height) <- getWindowSize (getWindow frame)
        
        VerticalShader.parameters shader
            identityLH
            (getView camera)
            (getHalfOrtho . getOpenGL $ frame)
            (fromIntegral height)
            (textureUnit horiblur)
        
        render (getSmallWindow frame)
        
        return (True,Sub VerticalBlur frame)
    
    render (Sub UpSample frame) = do
        render (getUpSample frame)
        
        beginScene 1 1 0 1
        
        let camera = getCamera frame
            Just vertblur = getVerticalTexture frame
            Just shader = getTextureShader frame
        
        TextureShader.parameters shader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (textureUnit vertblur)
        
        render (getFullScreen frame)
        
        return (True,Sub UpSample frame)
    
    render (Sub Scene frame) = do
        (width,height) <- getWindowSize (getWindow frame)
        
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        beginScene 1 0 1 1
        
        let camera = getCamera frame
            Just up = getUpSample frame
            Just shader = getTextureShader frame
        
        TextureShader.parameters shader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (textureUnit up)
        
        render (getFullScreen frame)
        
        return (True,Sub Scene frame)
