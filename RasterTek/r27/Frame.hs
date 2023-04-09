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
import Maths
import Model
import OpenGL
import ReflectionShader
import RenderTexture
import TextureShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getDemoRot :: GLfloat,
    getCamera :: Camera,
    getModel :: Maybe Model,
    getFloor :: Maybe Model,
    getRenderTex :: Maybe RenderTexture,
    getTextureShader :: Maybe TextureShader,
    getReflectionShader :: Maybe ReflectionShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    (modelscs,model) <- Model.initialize "asset/cube.txt" "asset/seafloor.tga" 1 True
    (floorscs,floor) <- Model.initialize "asset/floor.txt" "asset/blue01.tga" 2 False
    (rentexscs,rentex) <- RenderTexture.initialize width height 3
    (tshaderscs,tshader) <- TextureShader.initialize
    (rshaderscs,rshader) <- ReflectionShader.initialize
    return (and [modelscs,floorscs,rentexscs,tshaderscs,rshaderscs], Just $
        Frame window opengl 0 camera model floor rentex tshader rshader)

instance Render Frame where
    render frame = do
        (_,Sub _ frame') <- render (Sub ToTexture frame)
        
        (_,Sub _ frame'') <- render (Sub Scene frame')
        
        swapBuffers . getWindow $ frame
        
        let rotation = getDemoRot frame + 0.005
        
        return . (,) True $ frame'' {
            getDemoRot = if rotation > 2*pi then rotation - 2*pi else rotation }

instance Update Frame where
    update frame None = return (True, frame { getCamera = (getCamera frame) { getPosition = [0,0,-10] }})
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getFloor $ frame
        shutdown . getRenderTex $ frame
        shutdown . getTextureShader $ frame
        shutdown . getReflectionShader $ frame

data RenderType = Scene | ToTexture

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub ToTexture frame) = do
        render (getRenderTex frame)
            
        beginScene 0 0 0 1
        
        (_,camera) <- update (getCamera frame) (F (-1.5))
            
        let Just model = getModel frame
            Just shader = getTextureShader frame
            
        TextureShader.parameters shader
            (yRotationLH . getDemoRot $ frame)
            (getReflection camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
        
        render model

        (width,height) <- getWindowSize (getWindow frame)
        
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub ToTexture frame { getCamera = camera })
    
    render (Sub Scene frame) = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
            
        let Just model = getModel frame
            Just floor = getFloor frame
            Just rentex = getRenderTex frame
            Just tshader = getTextureShader frame
            Just rshader = getReflectionShader frame
            
        TextureShader.parameters tshader
            (yRotationLH . getDemoRot $ frame)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
        
        render model
            
        ReflectionShader.parameters rshader
            (translationLH [0,-1.5,0])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (getReflection camera)
            (modelTexture floor)
            (textureUnit rentex)

        render floor

        return (True,Sub Scene frame { getCamera = camera })
