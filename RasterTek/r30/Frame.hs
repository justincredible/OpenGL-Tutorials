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
import PointLight
import Maths
import Model
import OpenGL
import PointLightShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getLights :: [PointLight],
    getModel :: Maybe Model,
    getShader :: Maybe PointLightShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    lights <- sequence $ zipWith PointLight.initialize
        [[-3,1,3],[3,1,3],[-3,1,-3],[3,1,-3]]
        [[1,0,0,1],[0,1,0,1],[0,0,1,1],[1,1,1,1]]
    
    (modelscs,model) <- Model.initialize "asset/plane01.txt" "asset/stone01.tga" 1 True
    (shaderscs,shader) <- PointLightShader.initialize
    
    return (modelscs && shaderscs, Just $
        Frame window opengl camera lights model shader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just model = getModel frame
            Just shader = getShader frame
            positions = concat . map PointLight.getPosition . getLights $ frame
            colours = concat . map getDiffuse . getLights $ frame
            
        parameters shader
            identityLH
            (getView camera)
            (getProjection . getOpenGL $ frame)
            positions
            (modelTexture model)
            colours
        
        render model
        
        swapBuffers . getWindow $ frame
            
        return . (,) True $ frame

instance Update Frame where
    update frame None = return (True, frame {
        getCamera = (getCamera frame) {
            Camera.getPosition = [0,2,-12] }})
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getShader $ frame
