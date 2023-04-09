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

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getFloor :: Maybe Model,
    getBillboard :: Maybe Model,
    getShader :: Maybe TextureShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    
    (floorscs,floor) <- Model.initialize "asset/floor.txt" "asset/grid01.tga" 1 False
    (boardscs,billboard) <- Model.initialize "asset/square.txt" "asset/seafloor.tga" 2 False
    (shaderscs,shader) <- TextureShader.initialize
    
    return (floorscs && boardscs && shaderscs, Just $
        Frame window opengl camera floor billboard shader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just floor = getFloor frame
            Just billboard = getBillboard frame
            Just shader = getShader frame
            
        parameters shader
            identityLH
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture floor)
        
        render floor
        
        let position = getPosition camera
            angle = atan2 (-head position) (-last position)
            rotation = yRotationLH angle
            translate = translationLH [0,1.5,0]
            
        parameters shader
            (concat . matmult4 (unconcat translate) $ unconcat rotation)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture billboard)
        
        render billboard
        
        swapBuffers . getWindow $ frame
            
        return (True,frame { getCamera = camera })

instance Update Frame where
    update frame (LF position) = return (True, frame {
        getCamera = (getCamera frame) {
            getPosition = position }})
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getFloor $ frame
        shutdown . getBillboard $ frame
        shutdown . getShader $ frame

unconcat [] = []
unconcat xs = ((:) . fst <*> unconcat . snd) . splitAt 4 $ xs
