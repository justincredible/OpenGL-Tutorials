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
import TextureShader
import TransparentShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getModel1 :: Maybe Model,
    getModel2 :: Maybe Model,
    getTextureShader :: Maybe TextureShader,
    getTransparentShader :: Maybe TransparentShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    (model1scs,model1) <- Model.initialize "asset/square.txt" "asset/dirt01.tga" 1 True
    (model2scs,model2) <- Model.initialize "asset/square.txt" "asset/stone01.tga" 2 True
    (tshaderscs,tshader) <- TextureShader.initialize
    (rshaderscs,rshader) <- TransparentShader.initialize
    return (and [model1scs,model2scs,tshaderscs,rshaderscs], Just $
        Frame window opengl camera model1 model2 tshader rshader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just model1 = getModel1 frame
            Just model2 = getModel2 frame
            Just tshader = getTextureShader frame
            Just rshader = getTransparentShader frame
        
        TextureShader.parameters tshader
            identityLH
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model1)

        render model1
        
        turnOnAlphaBlending
        
        TransparentShader.parameters rshader
            (translationLH [1,0,-1])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model2)
            0.5

        render model2
        
        turnOffAlphaBlending
        
        swapBuffers . getWindow $ frame
        
        return . (,) True $ frame {
            getCamera = camera }

instance Update Frame where
    update frame None = return (True, frame { getCamera = (getCamera frame) { getPosition = [0,0,-5] }})
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel1 $ frame
        shutdown . getModel2 $ frame
        shutdown . getTextureShader $ frame
        shutdown . getTransparentShader $ frame
