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
import Light
import Maths
import Model
import OpenGL
import ProjectionShader
import Texture
import Viewpoint

nearPlane = 1
farPlane = 100
shadowMap = 1024

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getLight :: Light,
    getViewpoint :: Viewpoint,
    getCube :: Maybe Model,
    getGround :: Maybe Model,
    getProjectTex :: Maybe Texture,
    getShader :: Maybe ProjectionShader }
    deriving (Eq, Show)

initialize window width height = do
    let downSample = quot shadowMap 2
    
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render
    light <- Light.initialize
    (_,viewpoint) <- Viewpoint.initialize
        >>= flip update (LF2 [2,5,-2] [0,0,0]) -- view
        >>= flip update (F4 (pi/2) 1 0.1 100) . snd -- projection

    (cubescs,cube) <- Model.initialize "asset/cube.txt" "asset/seafloor.tga" 1 True
    (groundscs,ground) <- Model.initialize "asset/floor.txt" "asset/stone01.tga" 2 True
    (texscs,projectex) <- Texture.initialize "asset/dx11.tga" 3 True
    (shaderscs,shader) <- ProjectionShader.initialize
    
    return (and [cubescs,groundscs,texscs,shaderscs], Just $
        Frame window opengl camera {
                getBaseView = getView camera,
                getPosition = [0,7,-10],
                getRotation = [7*pi/36,0,0] }
            light { getDirection = [0,-0.75,0,5] }
            viewpoint cube ground projectex shader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just cube = getCube frame
            Just floor = getGround frame
            Just projectex = getProjectTex frame
            Just shader = getShader frame
        
        parameters shader
            (translationLH [0,1,0])
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix . getViewpoint $ frame)
            (projectionMatrix . getViewpoint $ frame)
            (modelTexture floor)
            (textureUnit projectex)
            (getDirection . getLight $ frame)
            (getAmbient . getLight $ frame)
            (getDiffuse . getLight $ frame)
        
        render floor
        
        parameters shader
            (translationLH [0,2,0])
            (Camera.getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (viewMatrix . getViewpoint $ frame)
            (projectionMatrix . getViewpoint $ frame)
            (modelTexture cube)
            (textureUnit projectex)
            (getDirection . getLight $ frame)
            (getAmbient . getLight $ frame)
            (getDiffuse . getLight $ frame)
        
        render cube
        
        swapBuffers . getWindow $ frame
        
        return (True, frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getCube $ frame
        shutdown . getGround $ frame
        shutdown . getProjectTex $ frame
        shutdown . getShader $ frame
