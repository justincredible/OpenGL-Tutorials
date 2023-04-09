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
import GlassShader
import Maths
import Model
import OpenGL
import RenderTexture
import TextureShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getDemoRot :: GLfloat,
    getModel :: Maybe Model,
    getWindowMdl :: Maybe Model,
    getRenderTex :: Maybe RenderTexture,
    getTextureShader :: Maybe TextureShader,
    getGlassShader :: Maybe GlassShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    
    (modelscs,model) <- Model.initialize "asset/cube.txt" "asset/seafloor.tga" 1 "asset/bump03.tga" 2 False
    (windowscs,windowmdl) <- Model.initialize "asset/square.txt" "asset/glass01.tga" 3 "asset/bump03.tga" 4 False
    (rentexscs,rentex) <- RenderTexture.initialize width height 5
    (tshaderscs,tshader) <- TextureShader.initialize
    (gshaderscs,gshader) <- GlassShader.initialize
    
    return (and [modelscs,windowscs,rentexscs,tshaderscs,gshaderscs], Just $
        Frame window opengl camera 0 model windowmdl rentex tshader gshader)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,Sub _ frame') <- render (Sub ToTexture frame)
        
        render (Sub Scene frame')
            
        return . (,) True $ frame'

instance Update Frame where
    update frame None = let rotation = getDemoRot frame + 0.005 in
        return (True, frame {
            getCamera = (getCamera frame) { Camera.getPosition = [0,0,-10] },
            getDemoRot = if rotation > 2*pi then rotation - 2*pi else rotation })
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getWindowMdl $ frame
        shutdown . getRenderTex $ frame
        shutdown . getTextureShader $ frame
        shutdown . getGlassShader $ frame

data RenderType = Scene | ToTexture

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub ToTexture frame) = do
        render (getRenderTex frame)
            
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
            
        let Just model = getModel frame
            Just shader = getTextureShader frame
            
        TextureShader.parameters shader
            (yRotationLH . getDemoRot $ frame)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
        
        render model

        (width,height) <- getWindowSize (getWindow frame)
        
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub ToTexture frame { getCamera = camera })

    render (Sub Scene frame) = do    
        beginScene 0 0 0 1
        
        let camera = getCamera frame
            Just model = getModel frame
            Just window = getWindowMdl frame
            Just rentex = getRenderTex frame
            Just tshader = getTextureShader frame
            Just gshader = getGlassShader frame
            
        TextureShader.parameters tshader
            (yRotationLH . getDemoRot $ frame)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
        
        render model
        
        GlassShader.parameters gshader
            (translationLH [0,0,-1.5])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture window)
            (normalTexture window)
            (textureUnit rentex)
            0.01
        
        render window
        
        swapBuffers . getWindow $ frame
        
        return (True,Sub Scene frame)
