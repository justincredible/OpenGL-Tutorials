module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Bitmap
import Camera
import FadeShader
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Maths
import Model
import OpenGL
import RenderTexture
import TextureShader

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getDemoRot :: GLfloat,
    getCamera :: Camera,
    getModel :: Maybe Model,
    getBitmap :: Maybe Bitmap,
    getRenderTex :: Maybe RenderTexture,
    getTextureShader :: Maybe TextureShader,
    getFadeShader :: Maybe FadeShader,
    getFadeInfo :: (Bool, GLfloat, GLfloat, GLfloat) }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height
    camera <- fmap snd $ Camera.initialize >>= render
    (modelscs,model) <- Model.initialize "asset/cube.txt" "asset/seafloor.tga" 1 True
    (bmscs,bitmap) <- Bitmap.initialize width height width height
    (rentexscs,rentex) <- RenderTexture.initialize (fromIntegral width) (fromIntegral height) 2
    (tshaderscs,tshader) <- TextureShader.initialize
    (fshaderscs,fshader) <- FadeShader.initialize
    
    return (and [modelscs,bmscs,rentexscs,tshaderscs,fshaderscs], Just $
        Frame window opengl 0 camera model bitmap rentex tshader fshader (False, 3000, 0, 0))

instance Render Frame where
    render frame = do
        let (done,_,_,_) = getFadeInfo frame
        
        let rotation = getDemoRot frame + 0.005
        
        if done
        then do
            render (Sub Scene frame)
            
            return . (,) True $ frame {
                getDemoRot = if rotation > 2*pi then rotation - 2*pi else rotation }
        else do
            (_,Sub _ frame') <- render (Sub ToTexture frame)
        
            render (Sub FadeIn frame')
        
            return . (,) True $ frame' {
                getDemoRot = if rotation > 2*pi then rotation - 2*pi else rotation }

instance Update Frame where
    update frame None = return (True, frame { getCamera = (getCamera frame) { getPosition = [0,0,-10] }})
    update frame (F frametime) = do
        let camera = getCamera frame
            (done, fadein, accum, percent) = getFadeInfo frame
            accumtime = accum + frametime
            fadepc = if accumtime < fadein then accumtime/fadein else 1
            done' = if accumtime < fadein then done else True
            
        return (True,frame {
            getCamera = camera { getPosition = [0,0,-10] },
            getFadeInfo = (done', fadein, accumtime, fadepc) })
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getModel $ frame
        shutdown . getBitmap $ frame
        shutdown . getRenderTex $ frame
        shutdown . getTextureShader $ frame
        shutdown . getFadeShader $ frame

data RenderType = Scene | FadeIn | ToTexture

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
    
    render (Sub FadeIn frame) = do
        beginScene 0 0 0 1
        
        let camera = getCamera frame
            Just rentex = getRenderTex frame
            Just shader = getFadeShader frame
            (_,_,_,fadepc) = getFadeInfo frame
        
        turnZBufferOff
        
        FadeShader.parameters shader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (textureUnit rentex)
            fadepc
        
        update (getBitmap frame) (II 0 0) >>= render . snd
        
        turnZBufferOn
        
        swapBuffers . getWindow $ frame

        return (True,Sub FadeIn frame)
    
    render (Sub Scene frame) = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
            
        let Just model = getModel frame
            Just rentex = getRenderTex frame
            Just tshader = getTextureShader frame
            
        TextureShader.parameters tshader
            (yRotationLH . getDemoRot $ frame)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
        
        render model
        
        swapBuffers . getWindow $ frame

        return (True,Sub Scene frame)
