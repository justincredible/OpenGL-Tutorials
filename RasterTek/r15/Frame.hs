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
import Text2D

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getText2D :: Maybe Text2D }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd . join . fmap render $ Camera.initialize
    (success,text2D) <- Text2D.initialize width height (getView camera)
    return (success, Just $ Frame window opengl camera text2D)

instance Render Frame where
    render frame@(Frame window opengl camera (Just text2D)) = do
        beginScene 0 0 0 1
        
        turnZBufferOff
        
        render (Text text2D identityLH (getOrthographic opengl))
        
        turnZBufferOn
        
        swapBuffers window
        
        return . (,) True $ frame

instance Update Frame where
    update frame param@(II mx my) = do
        (success,text2D) <- update (getText2D frame) param
        return . (,) success $ frame { getText2D = text2D }
    update frame param@(IF fps frametime) = do
        (success,text2D) <- update (getText2D frame) param
        return . (,) success $ frame {
            getText2D = text2D,
            getCamera = (getCamera frame) { getPosition = [0,0,-10] } }
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown (Frame _ _ _ text2D) = do
        shutdown text2D
