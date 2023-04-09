module Frame (Frame,Frame.initialize) where

import Control.Monad
import Data.Foldable
import Graphics.GL
import Graphics.UI.GLFW hiding (init)
import Linear.Matrix
import Linear.V4

import Bitmap
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
import TextureShader
import Text2D

nearPlane = 0.1
farPlane = 1000

data Frame = Frame {
    getChecking :: Bool,
    getWindow :: Window,
    getOpenGL :: OpenGL,
    getCamera :: Camera,
    getLight :: Light,
    getText2D :: Maybe Text2D,
    getModel :: Maybe Model,
    getBitmap :: Maybe Bitmap,
    getLightShader :: Maybe LightShader,
    getTextureShader :: Maybe TextureShader }
    deriving (Eq, Show)

initialize window width height = do
    let downWidth = fromIntegral $ quot width 2
        downHeight = fromIntegral $ quot height 2
    
    opengl <- OpenGL.initialize window width height nearPlane farPlane
    camera <- fmap snd $ Camera.initialize >>= render
    light <- Light.initialize

    (t2dscs,text2D) <- Text2D.initialize width height (getView camera) -- texture unit 0
        >>= flip update (B False) . snd
    (mdlscs,model) <- Model.initialize "asset/sphere.txt" "asset/blue.tga" 1 False
    (bmscs,bitmap) <- Bitmap.initialize (fromIntegral width) (fromIntegral height) 32 32 "asset/mouse.tga" 2
    
    (lscs,lshader) <- LightShader.initialize
    (tscs,tshader) <- TextureShader.initialize
    
    return (and [t2dscs,mdlscs,bmscs,lscs,tscs], Just $
        Frame False window opengl camera { getBaseView = getView camera }
            light { getSpecular = [0,0,0,1] }
            text2D model bitmap lshader tshader)

instance Update Frame where
    update frame (BII leftdown mousex mousey) = do
        (_,bitmap) <- update (getBitmap frame) (II mousex mousey)
        
        let frame' = frame { getBitmap = bitmap }
        
        if not leftdown
        then return (True, frame' { getChecking = False })
        else if getChecking frame'
            then return (True,frame')
            else testIntersection (fromIntegral mousex) (fromIntegral mousey) frame' { getChecking = True }
            
    update frame _ = do
        putStrLn "Incorrect frame parameters."
        return (False,frame)

instance Render Frame where
    render frame = do
        beginScene 0 0 0 1
        
        (_,camera) <- render (getCamera frame)
        
        let Just model = getModel frame
            Just lshader = getLightShader frame
            Just bitmap = getBitmap frame
            Just tshader = getTextureShader frame
            Just text2D = getText2D frame
        
        LightShader.parameters lshader
            (translationLH [-5,1,5])
            (getView camera)
            (getProjection . getOpenGL $ frame)
            (modelTexture model)
            (getPosition camera)
            (getDirection . getLight $ frame)
            (getAmbient . getLight $ frame)
            (getDiffuse . getLight $ frame)
            (getSpecular . getLight $ frame)
            (getPower . getLight $ frame)
        
        render model
        
        turnZBufferOff
        
        turnOnAlphaBlending
        
        TextureShader.parameters tshader
            identityLH
            (getView camera)
            (getOrthographic . getOpenGL $ frame)
            (bitmapTexture bitmap)
        
        render bitmap
        
        render $ Text identityLH (getOrthographic . getOpenGL $ frame) text2D
        
        turnOffAlphaBlending
        
        turnZBufferOn
        
        swapBuffers . getWindow $ frame
        
        return (True, frame { getCamera = camera })

instance Shutdown Frame where
    shutdown frame = do
        shutdown . getText2D $ frame
        shutdown . getModel $ frame
        shutdown . getBitmap $ frame
        shutdown . getLightShader $ frame
        shutdown . getTextureShader $ frame

testIntersection mousex mousey frame = do
    (_,camera) <- render (getCamera frame)
    
    (width,height) <- getWindowSize (getWindow frame)
    
    let projection = getProjection . getOpenGL $ frame
        pointx = (2*mousex/fromIntegral width - 1)/(projection!!0)
        pointy = (-2*mousey/fromIntegral height + 1)/(projection!!5)
        -- view matrix should be orthonormal by construction
        inview = transpose4 . getView $ camera
        direction = [
            pointx*inview!!0 + pointy*inview!!1 + inview!!2,
            pointx*inview!!4 + pointy*inview!!5 + inview!!6,
            pointx*inview!!8 + pointy*inview!!9 + inview!!10 ]
        invworld = listMatrixInverse $ translationLH [-5,1,5]
        origin = multiply4 (getPosition camera ++ [1]) invworld
        raydir = normalize . multiply3 direction . map init . init $ invworld
    
    (_,text2D) <- update (getText2D frame) . B $ raySphereIntersect (map (/last origin) $ take 3 origin) raydir 1
    
    return (True,frame { getText2D = text2D })

raySphereIntersect origin direction radius = let
    a = dot <*> id $ direction
    b = 2*dot direction origin
    c = (dot <*> id) origin - radius*radius
    in
    b*b - 4*a*c >= 0

listMatrixInverse = map toList . toList . transpose . inv44 . listMatrixToLinearMatrix

listMatrixToLinearMatrix [m11,m21,m31,m41,m12,m22,m32,m42,m13,m23,m33,m43,m14,m24,m34,m44] = V4
    (V4 m11 m12 m13 m14)
    (V4 m21 m22 m23 m24)
    (V4 m31 m32 m33 m34)
    (V4 m41 m42 m43 m44)
