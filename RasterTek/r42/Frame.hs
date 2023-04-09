module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Camera
import DepthShader
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import HorizontalShader
import Maths
import Model
import OpenGL
import OrthoWindow
import RenderTexture
import ShadowLight
import ShadowShader
import SoftShadowShader
import TextureShader
import VerticalShader

nearPlane = 1
farPlane = 100
shadowMap = 1024

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getLight :: ShadowLight,
    getLightPosX :: Float,
    getCube :: Maybe Model,
    getSphere :: Maybe Model,
    getGround :: Maybe Model,
    getSmallWindow :: Maybe OrthoWindow,
    getFullScreen :: Maybe OrthoWindow,
    getRenderTex :: Maybe RenderTexture,
    getBlackWhite :: Maybe RenderTexture,
    getDownSample :: Maybe RenderTexture,
    getHorizontalBlur :: Maybe RenderTexture,
    getVerticalBlur :: Maybe RenderTexture,
    getUpSample :: Maybe RenderTexture,
    getDepthShader :: Maybe DepthShader,
    getShadowShader :: Maybe ShadowShader,
    getTextureShader :: Maybe TextureShader,
    getHorizontalShader :: Maybe HorizontalShader,
    getVerticalShader :: Maybe VerticalShader,
    getSoftShadowShader :: Maybe SoftShadowShader }
    deriving (Eq, Show)

initialize window width height = do
    let downSample = quot shadowMap 2
    
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render
    (_,light) <- ShadowLight.initialize >>= flip update (FF nearPlane farPlane)

    (cubescs,Just cube) <- Model.initialize "asset/cube.txt" "asset/wall01.tga" 1 True
    (spherescs,Just sphere) <- Model.initialize "asset/sphere.txt" "asset/ice01.tga" 2 True
    (groundscs,Just ground) <- Model.initialize "asset/plane01.txt" "asset/metal001.tga" 3 True
    (smallscs,small) <- OrthoWindow.initialize (quot width 2) (quot height 2)
    (fullscs,full) <- OrthoWindow.initialize width height
    (rentexscs,rentex) <- RenderTexture.initialize shadowMap shadowMap 4
    (blkwhtscs,blkwht) <- RenderTexture.initialize shadowMap shadowMap 5
    (downscs,downsample) <- RenderTexture.initialize downSample downSample 6
    (horiscs,horizontal) <- RenderTexture.initialize downSample downSample 7
    (vertscs,vertical) <- RenderTexture.initialize downSample downSample 8
    (upscs,upsample) <- RenderTexture.initialize shadowMap shadowMap 9
    (dshaderscs,dshader) <- DepthShader.initialize
    (sshaderscs,sshader) <- ShadowShader.initialize
    (tshaderscs,tshader) <- TextureShader.initialize
    (hshaderscs,hshader) <- HorizontalShader.initialize
    (vshaderscs,vshader) <- VerticalShader.initialize
    (fshaderscs,fshader) <- SoftShadowShader.initialize
    
    return (and [cubescs,spherescs,groundscs,smallscs,fullscs,rentexscs,blkwhtscs,downscs,horiscs,vertscs,upscs,dshaderscs,sshaderscs,tshaderscs,hshaderscs,vshaderscs,fshaderscs], Just $
        Frame window opengl camera { getBaseView = getView camera }
            light (-5)
            (Just cube { Model.getPosition = [-2,2,0] })
            (Just sphere { Model.getPosition = [2,2,0] })
            (Just ground { Model.getPosition = [0,1,0] })
            small full
            rentex blkwht downsample horizontal vertical upsample
            dshader sshader tshader hshader vshader fshader)

instance Render Frame where
    render frame = do
        (success1,Sub _ frame') <- render (Sub ToTexture frame)
        
        (success2,Sub _ frame'') <- render (Sub BlackWhite frame')
        
        render (Sub Down frame'')
        
        render (Sub HBlur frame'')
        
        render (Sub VBlur frame'')
        
        render (Sub Up frame'')
        
        render (Sub Scene frame'')
        
        swapBuffers . getWindow $ frame''
        
        return (success1 && success2, frame'')

instance Update Frame where
    update frame (LFLF position rotation) = do
        let lightpos = if (getLightPosX frame) + 0.05 > 5 then -5 else (getLightPosX frame) + 0.05
        
        return (True, frame {
            getLightPosX = lightpos,
            getLight = (getLight frame) { ShadowLight.getPosition = [lightpos,8,-5] },
            getCamera = (getCamera frame) {
                Camera.getPosition = position,
                getRotation = rotation }})

    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getCube $ frame
        shutdown . getSphere $ frame
        shutdown . getGround $ frame
        shutdown . getSmallWindow $ frame
        shutdown . getFullScreen $ frame
        shutdown . getRenderTex $ frame
        shutdown . getBlackWhite $ frame
        shutdown . getDownSample $ frame
        shutdown . getHorizontalBlur $ frame
        shutdown . getVerticalBlur $ frame
        shutdown . getUpSample $ frame
        shutdown . getDepthShader $ frame
        shutdown . getShadowShader $ frame
        shutdown . getTextureShader $ frame
        shutdown . getHorizontalShader $ frame
        shutdown . getVerticalShader $ frame
        shutdown . getSoftShadowShader $ frame

data RenderType = Scene | ToTexture | BlackWhite | Down | HBlur | VBlur | Up 

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub ToTexture frame) = do
        render (getRenderTex frame)
        
        beginScene 0 0 0 1
        
        let Just shader = getDepthShader frame
            Just cube = getCube frame
            Just sphere = getSphere frame
            Just ground = getGround frame
        
        (_,light) <- update (getLight frame) None
        
        DepthShader.parameters shader
            (translationLH . Model.getPosition $ cube)
            (viewMatrix light)
            (projectionMatrix light)
        
        render cube
        
        DepthShader.parameters shader
            (translationLH . Model.getPosition $ sphere)
            (viewMatrix light)
            (projectionMatrix light)
        
        render sphere
        
        DepthShader.parameters shader
            (translationLH . Model.getPosition $ ground)
            (viewMatrix light)
            (projectionMatrix light)
        
        render ground
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub ToTexture frame { getLight = light })

    render (Sub BlackWhite frame) = do
        render (getBlackWhite frame)
        
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let light = getLight frame
            Just shader = getShadowShader frame
            Just cube = getCube frame
            Just sphere = getSphere frame
            Just ground = getGround frame
            Just rentex = getRenderTex frame
        
        ShadowShader.parameters shader
            (translationLH . Model.getPosition $ cube)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix light)
            (projectionMatrix light)
            (ShadowLight.getPosition light)
            (textureUnit rentex)
        
        render cube
        
        ShadowShader.parameters shader
            (translationLH . Model.getPosition $ sphere)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix light)
            (projectionMatrix light)
            (ShadowLight.getPosition light)
            (textureUnit rentex)
        
        render sphere
        
        ShadowShader.parameters shader
            (translationLH . Model.getPosition $ ground)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix light)
            (projectionMatrix light)
            (ShadowLight.getPosition light)
            (textureUnit rentex)
        
        render ground
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub BlackWhite frame { getCamera = camera })

    render (Sub Down frame) = do
        render (getDownSample frame)
        
        beginScene 0 0 0 1
        
        let camera = getCamera frame
            light = getLight frame
            Just shader = getTextureShader frame
            Just small = getSmallWindow frame
            Just rentex = getBlackWhite frame
        
        turnZBufferOff
        
        TextureShader.parameters shader
            identityLH
            (Camera.getBaseView camera)
            (OpenGL.getHalfOrtho . getOpenGL $ frame)
            (textureUnit rentex)
        
        render small
        
        turnZBufferOn
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub Down frame)

    render (Sub HBlur frame) = do
        render (getHorizontalBlur frame)
        
        beginScene 0 0 0 1
        
        let screenX = fromIntegral $ quot shadowMap 2
            camera = getCamera frame
            light = getLight frame
            Just shader = getHorizontalShader frame
            Just small = getSmallWindow frame
            Just rentex = getDownSample frame
        
        turnZBufferOff
        
        HorizontalShader.parameters shader
            identityLH
            (Camera.getBaseView camera)
            (OpenGL.getHalfOrtho . getOpenGL $ frame)
            screenX
            (textureUnit rentex)
        
        render small
        
        turnZBufferOn
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub HBlur frame)

    render (Sub VBlur frame) = do
        render (getVerticalBlur frame)
        
        beginScene 0 0 0 1
        
        let screenY = fromIntegral $ quot shadowMap 2
            camera = getCamera frame
            light = getLight frame
            Just shader = getVerticalShader frame
            Just small = getSmallWindow frame
            Just rentex = getHorizontalBlur frame
        
        turnZBufferOff
        
        VerticalShader.parameters shader
            identityLH
            (Camera.getBaseView camera)
            (OpenGL.getHalfOrtho . getOpenGL $ frame)
            screenY
            (textureUnit rentex)
        
        render small
        
        turnZBufferOn
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub VBlur frame)

    render (Sub Up frame) = do
        render (getUpSample frame)
        
        beginScene 0 0 0 1
        
        let camera = getCamera frame
            light = getLight frame
            Just shader = getTextureShader frame
            Just full = getFullScreen frame
            Just rentex = getVerticalBlur frame
        
        turnZBufferOff
        
        TextureShader.parameters shader
            identityLH
            (Camera.getBaseView camera)
            (OpenGL.getOrthographic . getOpenGL $ frame)
            (textureUnit rentex)
        
        render full
        
        turnZBufferOn
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub Up frame)
    
    render (Sub Scene frame) = do
        beginScene 0 0 0 1
        
        let camera = getCamera frame
            light = getLight frame
            Just shader = getSoftShadowShader frame
            Just cube = getCube frame
            Just sphere = getSphere frame
            Just ground = getGround frame
            Just rentex = getUpSample frame
        
        SoftShadowShader.parameters shader
            (translationLH . Model.getPosition $ cube)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (ShadowLight.getPosition light)
            (modelTexture cube)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
        
        render cube
        
        SoftShadowShader.parameters shader
            (translationLH . Model.getPosition $ sphere)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (ShadowLight.getPosition light)
            (modelTexture sphere)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
        
        render sphere
        
        SoftShadowShader.parameters shader
            (translationLH . Model.getPosition $ ground)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (ShadowLight.getPosition light)
            (modelTexture ground)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
        
        render ground
        
        return (True,Sub Scene frame)
