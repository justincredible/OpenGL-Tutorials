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
import OpenGL
import ParticleShader
import ParticleSystem

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getSystem :: Maybe ParticleSystem,
    getShader :: Maybe ParticleShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render

    (systemscs,system) <- ParticleSystem.initialize "asset/star.tga" 1 True
    (shaderscs,shader) <- ParticleShader.initialize
    
    return (systemscs && shaderscs, Just $
        Frame window opengl camera system shader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just system = getSystem frame
            Just shader = getShader frame
        
        turnOnAlphaBlending
        
        parameters shader
            identityLH
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (systemTexture system)
        
        render system
        
        turnOffAlphaBlending
        
        swapBuffers . getWindow $ frame
        
        return (True,frame)

instance Update Frame where
    update frame param@(F time) = do
        (success,system) <- update (getSystem frame) param >>= flip update None . snd
        
        return (success, frame {
            getSystem = system,
            getCamera = (getCamera frame) {
                getPosition = [0,-2,-10] }})

    update frame None = return (True, frame {
        getCamera = (getCamera frame) {
            getPosition = [0,0,-10] }})

    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getSystem $ frame
        shutdown . getShader $ frame
