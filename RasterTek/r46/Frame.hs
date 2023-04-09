module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Bitmap
import Camera
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import GlowMapShader
import GlowShader
import HorizontalShader
import Maths
import OpenGL
import OrthoWindow
import RenderTexture
import TextureShader
import VerticalShader

nearPlane = 0.1
farPlane = 1000

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getBitmap :: Maybe Bitmap,
    getSmallWindow :: Maybe OrthoWindow,
    getFullScreen :: Maybe OrthoWindow,
    getRenderTex :: Maybe RenderTexture,
    getDownSample :: Maybe RenderTexture,
    getHorizontalBlur :: Maybe RenderTexture,
    getVerticalBlur :: Maybe RenderTexture,
    getUpSample :: Maybe RenderTexture,
    getTextureShader :: Maybe TextureShader,
    getHoriBlurShader :: Maybe HorizontalShader,
    getVertBlurShader :: Maybe VerticalShader,
    getGlowShader :: Maybe GlowShader,
    getGlowMapShader :: Maybe GlowMapShader }
    deriving (Eq, Show)

initialize window width height = do
    let downWidth = fromIntegral $ quot width 2
        downHeight = fromIntegral $ quot height 2
    
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render

    (bmscs,bitmap) <- Bitmap.initialize width height 256 32 "asset/test.tga" 1 "asset/glowmap.tga" 2
    
    (smscs,small) <- OrthoWindow.initialize downWidth downHeight
    (fsscs,full) <- OrthoWindow.initialize width height
    
    (rtscs,rentex) <- RenderTexture.initialize (fromIntegral width) (fromIntegral height) 3
    (dsscs,downsample) <- RenderTexture.initialize downWidth downHeight 4
    (hbscs,horiblur) <- RenderTexture.initialize downWidth downHeight 5
    (vbscs,vertblur) <- RenderTexture.initialize downWidth downHeight 6
    (usscs,upsample) <- RenderTexture.initialize (fromIntegral width) (fromIntegral height) 7
    
    (tscs,tshader) <- TextureShader.initialize
    (hscs,hshader) <- HorizontalShader.initialize
    (vscs,vshader) <- VerticalShader.initialize
    (gscs,gshader) <- GlowShader.initialize
    (gmscs,gmshader) <- GlowMapShader.initialize
    
    return (and [bmscs,smscs,fsscs,rtscs,dsscs,hbscs,vbscs,usscs,tscs,hscs,vscs,gscs,gmscs], Just $
        Frame window opengl camera { getBaseView = getView camera }
            bitmap small full
            rentex downsample horiblur vertblur upsample
            tshader hshader vshader gshader gmshader)

instance Render Frame where
    render frame = do
        render (Sub ToTexture frame)
        
        render (Sub Down frame)
        
        render (Sub HBlur frame)
        
        render (Sub VBlur frame)
        
        render (Sub Up frame)
        
        render (Sub UI frame)
        
        render (Sub Scene frame)
        
        swapBuffers . getWindow $ frame
        
        return (True, frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getBitmap $ frame
        shutdown . getSmallWindow $ frame
        shutdown . getFullScreen $ frame
        shutdown . getRenderTex $ frame
        shutdown . getDownSample $ frame
        shutdown . getHorizontalBlur $ frame
        shutdown . getVerticalBlur $ frame
        shutdown . getUpSample $ frame
        shutdown . getTextureShader $ frame
        shutdown . getHoriBlurShader $ frame
        shutdown . getVertBlurShader $ frame
        shutdown . getGlowShader $ frame
        shutdown . getGlowMapShader $ frame

data RenderType = Scene | ToTexture | Down | HBlur | VBlur | Up | UI

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub ToTexture frame) = do
        render (getRenderTex frame)
        
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just bitmap = getBitmap frame
            Just shader = getGlowMapShader frame
        
        turnZBufferOff
        
        GlowMapShader.parameters shader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (bitmapTexture bitmap)
            (bitmapGlowMap bitmap)
        
        update bitmap (II 100 100) >>= render . snd
        
        turnZBufferOn
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub ToTexture frame)

    render (Sub Down frame) = do
        render (getDownSample frame)
        
        beginScene 1 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just window = getSmallWindow frame
            Just shader = getTextureShader frame
            Just rentex = getRenderTex frame
        
        turnZBufferOff
        
        TextureShader.parameters shader
            identityLH
            (getView camera)
            (getHalfOrtho . getOpenGL $ frame)
            (textureUnit rentex)
        
        render window
        
        turnZBufferOn
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub Down frame)

    render (Sub HBlur frame) = do
        render (getHorizontalBlur frame)
        
        beginScene 0 1 0 1
        
        (_,camera) <- render (getCamera frame)
        
        (width,height) <- getWindowSize . getWindow $ frame
        
        let Just window = getSmallWindow frame
            Just shader = getHoriBlurShader frame
            Just rentex = getDownSample frame
            screenX = fromIntegral $ quot width 2
        
        turnZBufferOff
        
        HorizontalShader.parameters shader
            identityLH
            (getView camera)
            (getHalfOrtho . getOpenGL $ frame)
            screenX
            (textureUnit rentex)
        
        render window
        
        turnZBufferOn
        
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub HBlur frame)

    render (Sub VBlur frame) = do
        render (getVerticalBlur frame)
        
        beginScene 0 0 1 1
        
        (_,camera) <- render (getCamera frame)
        
        (width,height) <- getWindowSize . getWindow $ frame
        
        let Just window = getSmallWindow frame
            Just shader = getVertBlurShader frame
            Just rentex = getHorizontalBlur frame
            screenY = fromIntegral $ quot height 2
        
        turnZBufferOff
        
        VerticalShader.parameters shader
            identityLH
            (getView camera)
            (getHalfOrtho . getOpenGL $ frame)
            screenY
            (textureUnit rentex)
        
        render window
        
        turnZBufferOn
        
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub VBlur frame)

    render (Sub Up frame) = do
        render (getUpSample frame)
        
        beginScene 1 1 1 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just window = getFullScreen frame
            Just shader = getTextureShader frame
            Just rentex = getVerticalBlur frame
        
        turnZBufferOff
        
        TextureShader.parameters shader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (textureUnit rentex)
        
        render window
        
        turnZBufferOn
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub Up frame)

    render (Sub UI frame) = do
        render (getRenderTex frame)
        
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just bitmap = getBitmap frame
            Just shader = getTextureShader frame
        
        turnZBufferOff
        
        TextureShader.parameters shader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (bitmapTexture bitmap)
        
        render bitmap
        
        turnZBufferOn
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub UI frame)

    render (Sub Scene frame) = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just window = getFullScreen frame
            Just shader = getGlowShader frame
            Just rentex = getRenderTex frame
            Just glowmap = getUpSample frame
        
        turnZBufferOff
        
        GlowShader.parameters shader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (textureUnit rentex)
            (textureUnit glowmap)
            3
        
        render window
        
        turnZBufferOn
        
        return (True,Sub Scene frame)
