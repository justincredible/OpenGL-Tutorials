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
import Frustum
import Light
import LightShader
import Maths
import Model
import Modelist
import OpenGL
import Text2D

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getLight :: Light,
    getModelist :: Modelist,
    getText2D :: Maybe Text2D,
    getModel :: Maybe Model,
    getLightShader :: Maybe LightShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    light <- Light.initialize
    (_,modelist) <- Modelist.initialize 25
    (textscs,text2D) <- Text2D.initialize width height (getView camera) -- Text2D uses texture unit 0
    (modelscs,model) <- Model.initialize "asset/sphere.txt" "asset/seafloor.tga" 1 True
    (shaderscs,shader) <- LightShader.initialize
    return (and [textscs,modelscs,shaderscs], Just $
        Frame window opengl camera light modelist text2D model shader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let frustum = constructFrustum 1000
                (getProjection . getOpenGL $ frame)
                (getView $ camera)
        
        renderCount <- renderlist 0 frame camera frustum (getInfo . getModelist $ frame)
        
        turnZBufferOff
        
        (_,Just text2D) <- update (getText2D frame) (I renderCount)
        
        render $ Text identityLH (getOrthographic . getOpenGL $ frame) text2D
        
        turnZBufferOn
        
        swapBuffers . getWindow $ frame
        
        return . (,) True $ frame { getCamera = camera, getText2D = Just text2D }

renderlist count _ _ _ [] = return count
renderlist count frame camera frustum ((position,colour):info) =
    if not $ checkSphere position 1 frustum
    then renderlist count frame camera frustum info
    else do
        let Just model = getModel frame
            Just shader = getLightShader frame
            
        parameters shader
            (translationLH position)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
            (getDirection . getLight $ frame)
            colour
        
        render model
        renderlist (count+1) frame camera frustum info

instance Update Frame where
    update frame param@(F rotationY) = return (True, frame {
        getCamera = (getCamera frame) {
            getPosition = [0,0,-10],
            getRotation = [0,rotationY,0] }})
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getText2D $ frame
        shutdown . getModel $ frame
        shutdown . getLightShader $ frame
