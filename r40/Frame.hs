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
    getLight :: ShadowLight,
    getLightPosX :: GLfloat,
    getCube :: Maybe Model,
    getSphere :: Maybe Model,
    getGround :: Maybe Model,
    getRenderTex :: Maybe RenderTexture,
    getDepthShader :: Maybe DepthShader,
    getShadowShader :: Maybe ShadowShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render
    (_,light) <- ShadowLight.initialize >>= flip update (FF nearPlane farPlane)

    (cubescs,Just cube) <- Model.initialize "asset/cube.txt" "asset/wall01.tga" 1 True
    (spherescs,Just sphere) <- Model.initialize "asset/sphere.txt" "asset/ice01.tga" 2 True
    (groundscs,Just ground) <- Model.initialize "asset/plane01.txt" "asset/metal001.tga" 3 True
    (rentexscs,rentex) <- RenderTexture.initialize 1024 1024 4
    (dshaderscs,dshader) <- DepthShader.initialize
    (sshaderscs,sshader) <- ShadowShader.initialize
    
    return (and [cubescs,spherescs,groundscs,rentexscs,dshaderscs,sshaderscs], Just $
        Frame window opengl camera light (-5)
            (Just cube { Model.getPosition = [-2,2,0] })
            (Just sphere { Model.getPosition = [2,2,0] })
            (Just ground { Model.getPosition = [0,1,0] })
            rentex dshader sshader)

instance Render Frame where
    render frame = do
        (success1,Sub _ frame') <- render (Sub ToTexture frame)
        
        (success2,Sub _ frame'') <- render (Sub Scene frame')
        
        swapBuffers . getWindow $ frame''
        
        return (success1 && success2,frame'')

instance Update Frame where
    update frame (LFLF position rotation) = do
        let lightpos = if (getLightPosX frame) + 0.05 > 5 then -5 else (getLightPosX frame) + 0.05
            
        return (True, frame {
            getLightPosX = lightpos,
            getLight = (getLight frame) { ShadowLight.getPosition = [lightpos,8,-5] },
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
        shutdown . getRenderTex $ frame
        shutdown . getDepthShader $ frame
        shutdown . getShadowShader $ frame

data RenderType = Scene | ToTexture

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
    
    render (Sub Scene frame) = do
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
            (modelTexture cube)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
        
        render cube
        
        ShadowShader.parameters shader
            (translationLH . Model.getPosition $ sphere)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix light)
            (projectionMatrix light)
            (ShadowLight.getPosition light)
            (modelTexture sphere)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
        
        render sphere
        
        ShadowShader.parameters shader
            (translationLH . Model.getPosition $ ground)
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix light)
            (projectionMatrix light)
            (ShadowLight.getPosition light)
            (modelTexture ground)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
        
        render ground
        
        return (True,Sub Scene frame { getCamera = camera })
