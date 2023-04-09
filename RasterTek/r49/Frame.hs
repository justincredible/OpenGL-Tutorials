module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW

import Camera
import DepthShader
import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Maths
import Model
import OpenGL
import RenderTexture
import ShadowLight
import ShadowShader
import TransparentDepthShader
import Tree

nearPlane = 0.1
farPlane = 1000
shadowDimension = 1024
shadowNear = 1
shadowFar = 50

data Frame = Frame {
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getLightAngle :: Float,
    getLightPosX :: Float,
    getCamera :: Camera,
    getLight :: ShadowLight,
    getGround :: Maybe Model,
    getTree :: Maybe Tree,
    getRenderTex :: Maybe RenderTexture,
    getDepthShader :: Maybe DepthShader,
    getTransDepthShader :: Maybe TransparentDepthShader,
    getShadowShader :: Maybe ShadowShader }
    deriving (Eq, Show)

initialize window width height = do
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render
    (_,light) <- ShadowLight.initialize
        >>= flip update (F4 15 15 shadowNear shadowFar)
    
    (groundscs,Just ground) <- Model.initialize "asset/plane01.txt" "asset/dirt.tga" 1 True
    (treescs,Just tree) <- Tree.initialize 0.1
        "asset/trunk001.txt" "asset/trunk001.tga" 2
        "asset/leaf001.txt" "asset/leaf001.tga" 3
    (rentexscs,rentex) <- RenderTexture.initialize (fromIntegral shadowDimension) (fromIntegral shadowDimension) 4
    
    (dscs,dshader) <- DepthShader.initialize
    (tscs,tshader) <- TransparentDepthShader.initialize
    (sscs,sshader) <- ShadowShader.initialize
    
    return (and [groundscs,treescs,rentexscs,dscs,tscs,sscs], Just $
        Frame window opengl (3*pi/2) 9 camera { getBaseView = getView camera }
            light
            (Just ground { Model.getPosition = [0,1,0] })
            (Just tree { Tree.getPosition = [0,1,0] })
            rentex dshader tshader sshader)

instance Update Frame where
    update frame (LF2F position rotation frametime) = do
        let la = getLightAngle frame - 0.0005*frametime
            lightangle = if la < pi/2 then 3*pi/2 else la
            lightposx = if la < pi/2 then 9 else getLightPosX frame - 0.003*frametime
        
        return (True, frame {
            getLightAngle = lightangle,
            getLightPosX = lightposx,
            getLight = (getLight frame) {
                getDirection = [sin lightangle, cos lightangle, 0],
                ShadowLight.getPosition = [lightposx,10,1],
                getLookAt = [-lightposx,0,0] },
            getCamera = (getCamera frame) {
                Camera.getPosition = position,
                getRotation = rotation }})
            
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Render Frame where
    render frame = do
        (_,Sub _ frame') <- render (Sub ToTexture frame)
        
        (_,Sub _ frame'') <- render (Sub Scene frame')
        
        swapBuffers . getWindow $ frame''
        
        return (True, frame'')

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getGround $ frame
        shutdown . getTree $ frame
        shutdown . getRenderTex $ frame
        shutdown . getDepthShader $ frame
        shutdown . getTransDepthShader $ frame
        shutdown . getShadowShader $ frame

data RenderType = Scene | ToTexture

data SubRender = Sub RenderType Frame

instance Render SubRender where
    render (Sub ToTexture frame) = do
        render (getRenderTex frame)
        
        beginScene 0 0 0 1
        
        (_,light) <- update (getLight frame) None
        
        let Just ground = getGround frame
            Just tree = getTree frame
            Just dshader = getDepthShader frame
            Just tshader = getTransDepthShader frame

        DepthShader.parameters dshader
            (modelMatrix tree)
            (viewMatrix light)
            (orthographicMatrix light)
        
        render (treeTrunk tree)

        TransparentDepthShader.parameters tshader
            (modelMatrix tree)
            (viewMatrix light)
            (orthographicMatrix light)
            (treeTexture . treeLeaves $ tree)
        
        render (treeLeaves tree)

        DepthShader.parameters dshader
            (translationLH $ Model.getPosition ground)
            (viewMatrix light)
            (orthographicMatrix light)
        
        render ground
        
        (width,height) <- getWindowSize . getWindow $ frame
        setBackBufferRenderTarget (fromIntegral width) (fromIntegral height)
        
        return (True,Sub ToTexture frame { getLight = light })

    render (Sub Scene frame) = do
        beginScene 0 0.5 0.8 1
        
        (_,camera) <- render (getCamera frame)
        
        let light = getLight frame
            Just ground = getGround frame
            Just tree = getTree frame
            Just rentex = getRenderTex frame
            Just shader = getShadowShader frame
        
        ShadowShader.parameters shader
            (translationLH $ Model.getPosition ground)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (viewMatrix light)
            (orthographicMatrix light)
            (modelTexture ground)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
            (getDirection light)
        
        render ground
        
        ShadowShader.parameters shader
            (modelMatrix tree)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (viewMatrix light)
            (orthographicMatrix light)
            (treeTexture . treeTrunk $ tree)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
            (getDirection light)
        
        render (treeTrunk tree)
        
        turnOnAlphaBlending
        
        ShadowShader.parameters shader
            (modelMatrix tree)
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (viewMatrix light)
            (orthographicMatrix light)
            (treeTexture . treeLeaves $ tree)
            (textureUnit rentex)
            (getAmbient light)
            (getDiffuse light)
            (getDirection light)
        
        render (treeLeaves tree)
        
        turnOffAlphaBlending
        
        return (True,Sub Scene frame { getCamera = camera })
