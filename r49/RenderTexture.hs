module RenderTexture (RenderTexture,initialize,textureUnit) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

import Flow.Render
import Flow.Shutdown

data RenderTexture = RenderTexture {
    getWindowSize :: (Int,Int),
    getFrameBuffer :: GLuint,
    getDepthBuffer :: GLuint,
    getTexture :: GLuint,
    getTextureUnit :: GLint }
    deriving (Eq,Show)

initialize width height texUnit = do
    frameBuffer <- alloca $ (>>) . glGenFramebuffers 1 <*> peek
    glBindFramebuffer GL_FRAMEBUFFER frameBuffer

    let width' = fromIntegral width
        height' = fromIntegral height
    
    glActiveTexture $ GL_TEXTURE0 + fromIntegral texUnit
    
    rendertex <- alloca $ (>>) . glGenTextures 1 <*> peek
    glBindTexture GL_TEXTURE_2D rendertex
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) width' height' 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr

    let linear = fromIntegral GL_LINEAR
        clampedge = fromIntegral GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER linear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER linear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S clampedge
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T clampedge

    depthBuffer <- alloca $ (>>) . glGenRenderbuffers 1 <*> peek
    glBindRenderbuffer GL_RENDERBUFFER depthBuffer
    glRenderbufferStorage GL_RENDERBUFFER  GL_DEPTH_COMPONENT width' height'
    glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER depthBuffer

    glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 rendertex 0
    
    status <- glCheckFramebufferStatus GL_FRAMEBUFFER
    if status == fromIntegral GL_FRAMEBUFFER_COMPLETE
    then do
        glBindFramebuffer GL_FRAMEBUFFER 0
        return (True, Just $ RenderTexture (width,height) frameBuffer depthBuffer rendertex texUnit)
    else do
        shutdown $ RenderTexture (0,0) frameBuffer depthBuffer rendertex texUnit
        return (False,Nothing)

instance Render RenderTexture where
    render rentex = do
        let (width,height) = getWindowSize rentex
        glBindFramebuffer GL_FRAMEBUFFER (getFrameBuffer rentex)
        glViewport 0 0 (fromIntegral width) (fromIntegral height)
        return (True,rentex)

instance Shutdown RenderTexture where
    shutdown rentex = do
        with (getTexture rentex) $ glDeleteTextures 1
        with (getDepthBuffer rentex) $ glDeleteRenderbuffers 1
        with (getFrameBuffer rentex) $ glDeleteFramebuffers 1

textureUnit = getTextureUnit
