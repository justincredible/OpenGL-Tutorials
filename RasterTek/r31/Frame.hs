module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Audio
import Camera
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Maths
import OpenGL

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getAudio :: Audio }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    
    (success,audio) <- Audio.initialize "asset/sound02.wav"
    
    return (success, Just $
        Frame window opengl camera audio)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
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
    shutdown frame = shutdown . getAudio $ frame
