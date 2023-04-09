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
import LightShader
import Maths
import Model
import OpenGL
import RenderTexture
import RefractionShader
import WaterShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getLight :: Light,
    getGround :: Maybe Model,
    getWall :: Maybe Model,
    getBath :: Maybe Model,
    getWater :: Maybe Model,
    getReflecTex :: Maybe RenderTexture,
    getRefracTex :: Maybe RenderTexture,
    getLightShader :: Maybe LightShader,
    getRefractionShader :: Maybe RefractionShader,
    getWaterShader :: Maybe WaterShader,
    getWaterHeight :: GLfloat,
    getWaterTrans :: GLfloat }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    light <- Light.initialize
    
    (groundscs,ground) <- Model.initialize "asset/ground.txt" "asset/ground01.tga" 1 True
    (wallscs,wall) <- Model.initialize "asset/wall.txt" "asset/wall01.tga" 2 True
    (bathscs,bath) <- Model.initialize "asset/bath.txt" "asset/marble01.tga" 3 True
    (waterscs,water) <- Model.initialize "asset/water.txt" "asset/water01.tga" 4 True
    (refltexscs,refltex) <- RenderTexture.initialize (fromIntegral width) (fromIntegral height) 5
    (refrtexscs,refrtex) <- RenderTexture.initialize (fromIntegral width) (fromIntegral height) 6
    (lshaderscs,lshader) <- LightShader.initialize
    (rshaderscs,rshader) <- RefractionShader.initialize
    (wshaderscs,wshader) <- WaterShader.initialize
    
    return (and [groundscs,wallscs,bathscs,waterscs,refltexscs,refrtexscs,lshaderscs,rshaderscs,wshaderscs], Just $
        Frame window opengl camera light ground wall bath water refltex refrtex lshader rshader wshader 2.75 0)

instance Render Frame where
    render frame = do
        (_,Sub _ frame') <- render (Sub RefracTex frame)
        
        (_,Sub _ frame'') <- render (Sub ReflecTex frame')
        
        render (Sub Scene frame'')
            
        return . (,) True $ frame''

instance Update Frame where
    update frame None = do
        let camera = getCamera frame
            watertrans = getWaterTrans frame + 0.001
            
        return (True, frame {
            getCamera = camera { getPosition = [-10,6,-10], getRotation = [0,pi/4,0] },
            getWaterTrans = if watertrans  > 1 then watertrans - 1 else watertrans })
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getGround $ frame
        shutdown . getWall $ frame
        shutdown . getBath $ frame
        shutdown . getWater $ frame
        shutdown . getReflecTex $ frame
        shutdown . getRefracTex $ frame
        shutdown . getLightShader $ frame
        shutdown . getRefractionShader $ frame
        shutdown . getWaterShader $ frame

data RenderType = Scene | ReflecTex | RefracTex

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub RefracTex frame) = do
        render (getRefracTex frame)
            
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
            
        let Just bath = getBath frame
            Just shader = getRefractionShader frame
            
        RefractionShader.parameters shader
            (translationLH [0,2,0])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            [0,-1,0,getWaterHeight frame + 0.1]
            (modelTexture bath)
            (getDirection . getLight $ frame)
            (getAmbient . getLight $ frame)
            (getDiffuse . getLight $ frame)
        
        render bath

        (width,height) <- getWindowSize (getWindow frame)
        
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub RefracTex frame { getCamera = camera })
    
    render (Sub ReflecTex frame) = do
        render (getReflecTex frame)
            
        beginScene 0 0 0 1
        
        (_,camera) <- update (getCamera frame) (F (getWaterHeight frame))
        
        let Just wall = getWall frame
            Just shader = getLightShader frame
        
        LightShader.parameters shader
            (translationLH [0,6,8])
            (getReflection camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture wall)
            (getDirection . getLight $ frame)
            (getAmbient . getLight $ frame)
            (getDiffuse . getLight $ frame)
        
        render wall

        (width,height) <- getWindowSize (getWindow frame)
        
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)

        return (True,Sub ReflecTex frame { getCamera = camera })
    
    render (Sub Scene frame) = do
        beginScene 0 0 0 1
            
        let camera = getCamera frame
            Just ground = getGround frame
            Just wall = getWall frame
            Just bath = getBath frame
            Just water = getWater frame
            Just lshader = getLightShader frame
            Just wshader = getWaterShader frame
            Just reflectex = getReflecTex frame
            Just refractex = getRefracTex frame
            
        LightShader.parameters lshader
            (translationLH [0,1,0])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture ground)
            (getDirection . getLight $ frame)
            (getAmbient . getLight $ frame)
            (getDiffuse . getLight $ frame)
        
        render ground
            
        LightShader.parameters lshader
            (translationLH [0,6,8])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture wall)
            (getDirection . getLight $ frame)
            (getAmbient . getLight $ frame)
            (getDiffuse . getLight $ frame)
        
        render wall
            
        LightShader.parameters lshader
            (translationLH [0,2,0])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture bath)
            (getDirection . getLight $ frame)
            (getAmbient . getLight $ frame)
            (getDiffuse . getLight $ frame)
        
        render bath
            
        WaterShader.parameters wshader
            (translationLH [0,getWaterHeight frame,0])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (getReflection camera)
            (textureUnit reflectex)
            (textureUnit refractex)
            (modelTexture water)
            (getWaterTrans frame)
            0.01
        
        render water
        
        swapBuffers . getWindow $ frame

        return (True,Sub Scene frame)
