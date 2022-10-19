module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Camera
import ClipPlaneShader
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
    getModel :: Maybe Model,
    getShader :: Maybe ClipPlaneShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    (modelscs,model) <- Model.initialize "asset/triangle.txt" "asset/seafloor.tga" 1 False
    (shaderscs,shader) <- ClipPlaneShader.initialize
    return (modelscs && shaderscs, Just $
        Frame window opengl camera model shader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just model = getModel frame
            Just shader = getShader frame
        
        parameters shader
            identityLH
            (getView camera)
            (getProjection . getOpenGL $ frame)
            [0,-1,0,0]
            (modelTexture model)

        render model
        
        swapBuffers . getWindow $ frame
        
        return . (,) True $ frame { getCamera = camera }

instance Update Frame where
    update frame None = return (True, frame { getCamera = (getCamera frame) { getPosition = [0,0,-10] }})
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getShader $ frame
