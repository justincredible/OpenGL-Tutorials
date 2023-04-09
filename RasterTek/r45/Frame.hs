module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import BumpModel
import Camera
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Light
import Maths
import Model
import OpenGL
import ShaderManager

nearPlane = 0.1
farPlane = 1000
shadowMap = 1024

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getDemoRot :: Float,
    getCamera :: Camera,
    getLight :: Light,
    getModel1 :: Maybe Model,
    getModel2 :: Maybe Model,
    getModel3 :: Maybe BumpModel,
    getShaders :: ShaderManager }
    deriving (Eq, Show)

initialize window width height = do
    let downSample = quot shadowMap 2
    
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render
    light <- Light.initialize

    (m1scs,model1) <- Model.initialize "asset/cube.txt" "asset/marble.tga" 1 True
    (m2scs,model2) <- Model.initialize "asset/cube.txt" "asset/metal.tga" 2 True
    (m3scs,model3) <- BumpModel.initialize "asset/cube.txt" "asset/stone.tga" 3 True "asset/normal.tga" 4 True
    (shaderscs,shaders) <- ShaderManager.initialize
    
    return (and [m1scs,m2scs,m3scs,shaderscs], Just $
        Frame window opengl 0 camera { getBaseView = getView camera }
            light model1 model2 model3 shaders)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let demorot = getDemoRot frame + 0.005
            rotation = if demorot > 2*pi then demorot - 2*pi else demorot
            rotmat = unconcat $ yRotationLH rotation
            light = getLight frame
            Just model1 = getModel1 frame
            Just model2 = getModel2 frame
            Just model3 = getModel3 frame
            shaders = getShaders frame
        
        update shaders (ShaderTexture
            (concat . matmult4 (unconcat $ translationLH [-3.5,0,0]) $ rotmat)
            (getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (modelTexture model1))
        
        render model1
        
        update shaders (ShaderLight
            (concat . matmult4 (unconcat $ translationLH [0,0,0]) $ rotmat)
            (getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (modelTexture model2)
            (getPosition camera)
            (getDirection light)
            (getAmbient light)
            (getDiffuse light)
            (getSpecular light)
            (getPower light))
        
        render model2
        
        update shaders (ShaderBumpMap
            (concat . matmult4 (unconcat $ translationLH [3.5,0,0]) $ rotmat)
            (getView camera)
            (OpenGL.getProjection . getOpenGL $ frame)
            (bumpModelTexture model3)
            (bumpModelNormal model3)
            (getDirection light)
            (getDiffuse light))
        
        render model3
        
        swapBuffers . getWindow $ frame
        
        return (True, frame { getDemoRot = rotation })

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel1 $ frame
        shutdown . getModel2 $ frame
        shutdown . getModel3 $ frame
        shutdown . getShaders $ frame

unconcat [] = []
unconcat xs = (:) . fst <*> unconcat . snd $ splitAt 4 xs
