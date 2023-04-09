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
import Maths
import Model
import OpenGL
import RenderTexture
import ShadowLight
import ShadowShader

nearPlane = 1
farPlane = 100

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getLight1 :: ShadowLight,
    getLight2 :: ShadowLight,
    getCube :: Maybe Model,
    getSphere :: Maybe Model,
    getGround :: Maybe Model,
    getRenderTex1 :: Maybe RenderTexture,
    getRenderTex2 :: Maybe RenderTexture,
    getDepthShader :: Maybe DepthShader,
    getShadowShader :: Maybe ShadowShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render
    (_,light1) <- ShadowLight.initialize >>= flip update (FF nearPlane farPlane)
    (_,light2) <- ShadowLight.initialize >>= flip update (FF nearPlane farPlane)

    (cubescs,Just cube) <- Model.initialize "asset/cube.txt" "asset/wall01.tga" 1 True
    (spherescs,Just sphere) <- Model.initialize "asset/sphere.txt" "asset/ice01.tga" 2 True
    (groundscs,Just ground) <- Model.initialize "asset/plane01.txt" "asset/metal001.tga" 3 True
    (rentex1scs,rentex1) <- RenderTexture.initialize 1024 1024 4
    (rentex2scs,rentex2) <- RenderTexture.initialize 1024 1024 5
    (dshaderscs,dshader) <- DepthShader.initialize
    (sshaderscs,sshader) <- ShadowShader.initialize
    
    return (and [cubescs,spherescs,groundscs,rentex1scs,rentex2scs,dshaderscs,sshaderscs], Just $
        Frame window opengl camera light1 light2
            (Just cube { Model.getPosition = [-2,2,0] })
            (Just sphere { Model.getPosition = [2,2,0] })
            (Just ground { Model.getPosition = [0,1,0] })
            rentex1 rentex2 dshader sshader)

instance Render Frame where
    render frame = do
        (success1,Sub _ frame') <- render (Sub ToTexture1 frame)
        
        (success2,Sub _ frame'') <- render (Sub ToTexture2 frame')
        
        (success3,Sub _ frame''') <- render (Sub Scene frame'')
        
        swapBuffers . getWindow $ frame'''
        
        return (success1 && success2 && success3, frame''')

instance Update Frame where
    update frame (LFLF position rotation) = return (True, frame {
        getLight1 = (getLight1 frame) { ShadowLight.getPosition = [5,8,-5] },
        getLight2 = (getLight2 frame) { ShadowLight.getPosition = [-5,8,-5] },
        getCamera = (getCamera frame) {
            Camera.getPosition = position,
            getRotation = rotation }})

    update frame None = return (True, frame {
        getCamera = (getCamera frame) {
            Camera.getPosition = [0,0,-10] }})

    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getCube $ frame
        shutdown . getSphere $ frame
        shutdown . getGround $ frame
        shutdown . getRenderTex1 $ frame
        shutdown . getRenderTex2 $ frame
        shutdown . getDepthShader $ frame
        shutdown . getShadowShader $ frame

data RenderType = Scene | ToTexture1 | ToTexture2

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub ToTexture1 frame) = do
        render (getRenderTex1 frame)
        
        beginScene 0 0 0 1
        
        let Just shader = getDepthShader frame
            Just cube = getCube frame
            Just sphere = getSphere frame
            Just ground = getGround frame
        
        (_,light) <- update (getLight1 frame) None
        
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
        
        return (True,Sub ToTexture1 frame { getLight1 = light })

    render (Sub ToTexture2 frame) = do
        render (getRenderTex2 frame)
        
        beginScene 0 0 0 1
        
        let Just shader = getDepthShader frame
            Just cube = getCube frame
            Just sphere = getSphere frame
            Just ground = getGround frame
        
        (_,light) <- update (getLight2 frame) None
        
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
        
        return (True,Sub ToTexture2 frame { getLight2 = light })
    
    render (Sub Scene frame) = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let light1 = getLight1 frame
            light2 = getLight2 frame
            Just shader = getShadowShader frame
            Just cube = getCube frame
            Just sphere = getSphere frame
            Just ground = getGround frame
            Just rentex1 = getRenderTex1 frame
            Just rentex2 = getRenderTex2 frame
        
        ShadowShader.parameters shader
            (translationLH . Model.getPosition $ cube)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix light1 ++ viewMatrix light2)
            (projectionMatrix light1 ++ projectionMatrix light2)
            (ShadowLight.getPosition light1 ++ ShadowLight.getPosition light2)
            (modelTexture cube)
            [textureUnit rentex1,textureUnit rentex2]
            (getAmbient light1)
            (getDiffuse light1 ++ getDiffuse light2)
        
        render cube
        
        ShadowShader.parameters shader
            (translationLH . Model.getPosition $ sphere)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix light1 ++ viewMatrix light2)
            (projectionMatrix light1 ++ projectionMatrix light2)
            (ShadowLight.getPosition light1 ++ ShadowLight.getPosition light2)
            (modelTexture sphere)
            [textureUnit rentex1,textureUnit rentex2]
            (getAmbient light1)
            (getDiffuse light1 ++ getDiffuse light2)
        
        render sphere
        
        ShadowShader.parameters shader
            (translationLH . Model.getPosition $ ground)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix light1 ++ viewMatrix light2)
            (projectionMatrix light1 ++ projectionMatrix light2)
            (ShadowLight.getPosition light1 ++ ShadowLight.getPosition light2)
            (modelTexture ground)
            [textureUnit rentex1,textureUnit rentex2]
            (getAmbient light1)
            (getDiffuse light1 ++ getDiffuse light2)
        
        render ground
        
        return (True,Sub Scene frame { getCamera = camera })
