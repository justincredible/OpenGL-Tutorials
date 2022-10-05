module Frame (Frame,Frame.initialize,render,shutdown) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW
import Linear.Matrix
import Linear.Projection

import Camera
import Flow.Render
import Flow.Shutdown
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
        
        render (TextArg text2D identityLH (getOrthographic opengl))
        
        turnZBufferOn
        
        endScene window
        
        return . (,) True $ frame

instance Shutdown Frame where
    shutdown (Frame _ _ _ text2D) = shutdown text2D
