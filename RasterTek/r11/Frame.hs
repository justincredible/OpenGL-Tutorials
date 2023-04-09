module Frame (Frame,Frame.initialize,render,shutdown) where

import Control.Monad
import Data.Foldable
import Graphics.UI.GLFW
import Linear.Matrix

import Bitmap
import Camera
import Flow.Render
import Flow.Shutdown
import Maths
import OpenGL
import TextureShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getBitmap :: Maybe Bitmap,
    getTextureShader :: Maybe TextureShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- Camera.initialize
    (success,bitmap) <- Bitmap.initialize width height "asset/seafloor.tga" 0 256 256
    if not success
    then return (False, Nothing)
    else do
        (success, shader) <- TextureShader.initialize
        return (success, Just $ Frame window opengl camera bitmap shader)

instance Render Frame where
    render frame@(Frame window opengl camera (Just bitmap) (Just shader)) = do
        beginScene 0 0 0 1
        
        camera' <- fmap snd . render $ camera
        
        TextureShader.parameters shader
            identityLH
            (getView camera')
            (getOrthographic opengl)
            (bitmapTexUnit bitmap)
            
        turnZBufferOff
        
        update bitmap 100 100
        
        render bitmap
        
        turnZBufferOn

        endScene window
        
        return . (,) True $ frame { getCamera = camera' }

instance Shutdown Frame where
    shutdown (Frame _ _ _ bitmap shader) = do
        shutdown bitmap
        shutdown shader
