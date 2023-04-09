module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Camera
import FireShader
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Maths
import Model
import OpenGL

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getFrameTime :: GLfloat,
    getModel :: Maybe Model,
    getFireShader :: Maybe FireShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    
    (modelscs,model) <- Model.initialize
        "asset/square.txt"
        "asset/fire01.tga" 1 False
        "asset/noise01.tga" 2 True
        "asset/alpha01.tga" 3 False
    (shaderscs,shader) <- FireShader.initialize
    
    return (modelscs && shaderscs, Just $
        Frame window opengl camera 0 model shader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let frametime = if getFrameTime frame + 0.01 > 1000
                then 0
                else getFrameTime frame + 0.01
            Just model = getModel frame
            Just shader = getFireShader frame
        
        turnOnAlphaBlending
            
        parameters shader
            identityLH
            (getView camera)
            (getProjection . getOpenGL $ frame)
            frametime
            [1.3,2.1,2.3]
            [1..3]
            (modelTexture model)
            (noiseTexture model)
            (alphaTexture model)
            [[0.1,0.2],[0.1,0.3],[0.1,0.1]]
            0.8
            0.5
        
        render model
        
        turnOffAlphaBlending
        
        swapBuffers . getWindow $ frame
            
        return . (,) True $ frame { getCamera = camera, getFrameTime = frametime }

instance Update Frame where
    update frame None = return (True, frame {
        getCamera = (getCamera frame) {
            Camera.getPosition = [0,0,-10] }})
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getFireShader $ frame
