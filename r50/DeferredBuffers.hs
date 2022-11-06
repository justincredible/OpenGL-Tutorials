module DeferredBuffers (DeferredBuffers,initialize,colorUnit,normalUnit) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

import Flow.Render
import Flow.Shutdown

data DeferredBuffers = DeferredBuffers {
    getWindowSize :: (Int,Int),
    getFrameBuffer :: GLuint,
    getDepthBuffer :: GLuint,
    getColorTex :: GLuint,
    getColorUnit :: GLint,
    getNormalTex :: GLuint,
    getNormalUnit :: GLint }
    deriving (Eq,Show)

initialize width height clrUnit nrmlUnit = do
    frameBuffer <- alloca $ (>>) . glGenFramebuffers 1 <*> peek
    glBindFramebuffer GL_FRAMEBUFFER frameBuffer

    let width' = fromIntegral width
        height' = fromIntegral height
    
    glActiveTexture $ GL_TEXTURE0 + fromIntegral clrUnit
    
    colortex <- alloca $ (>>) . glGenTextures 1 <*> peek
    
    glBindTexture GL_TEXTURE_2D colortex
    
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA32F) width' height' 0 GL_RGBA GL_FLOAT nullPtr

    let linear = fromIntegral GL_LINEAR
        clampedge = fromIntegral GL_CLAMP_TO_EDGE
    
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER linear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER linear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S clampedge
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T clampedge

    glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 colortex 0
    
    --glBindTexture GL_TEXTURE_2D 0
    
    glActiveTexture $ GL_TEXTURE0 + fromIntegral nrmlUnit
    
    normaltex <- alloca $ (>>) . glGenTextures 1 <*> peek
    
    glBindTexture GL_TEXTURE_2D normaltex
    
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA32F) width' height' 0 GL_RGBA GL_FLOAT nullPtr
    
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER linear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER linear
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S clampedge
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T clampedge

    glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT1 normaltex 0
    
    withArray [GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1] $ glDrawBuffers 2

    depthBuffer <- alloca $ (>>) . glGenRenderbuffers 1 <*> peek
    glBindRenderbuffer GL_RENDERBUFFER depthBuffer
    glRenderbufferStorage GL_RENDERBUFFER  GL_DEPTH_COMPONENT width' height'
    glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER depthBuffer
    
    --glBindTexture GL_TEXTURE_2D 0
    
    status <- glCheckFramebufferStatus GL_FRAMEBUFFER
    if status == fromIntegral GL_FRAMEBUFFER_COMPLETE
    then do
        glBindFramebuffer GL_FRAMEBUFFER 0
        return (True, Just $ DeferredBuffers (width,height) frameBuffer depthBuffer colortex clrUnit normaltex nrmlUnit)
    else do
        shutdown $ DeferredBuffers (0,0) frameBuffer depthBuffer colortex clrUnit normaltex nrmlUnit
        return (False,Nothing)

instance Render DeferredBuffers where
    render rentex = do
        let (width,height) = getWindowSize rentex
        glBindFramebuffer GL_FRAMEBUFFER (getFrameBuffer rentex)
        glViewport 0 0 (fromIntegral width) (fromIntegral height)
        return (True,rentex)

instance Shutdown DeferredBuffers where
    shutdown rentex = do
        with (getColorTex rentex) $ glDeleteTextures 1
        with (getNormalTex rentex) $ glDeleteTextures 1
        with (getDepthBuffer rentex) $ glDeleteRenderbuffers 1
        with (getFrameBuffer rentex) $ glDeleteFramebuffers 1

colorUnit = getColorUnit
normalUnit = getNormalUnit
