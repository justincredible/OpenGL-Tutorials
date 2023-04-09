module Bitmap (Bitmap,Bitmap.initialize,bitmapTexture) where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Texture

data Bitmap = Bitmap {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getVertexCount :: GLsizei,
    getIndexCount :: GLsizei,
    getTexture :: Maybe Texture,
    getWindowSize :: (GLsizei, GLsizei),
    getBitmapSize :: (GLsizei, GLsizei),
    getPrevPos :: (GLint, GLint) }
    deriving (Eq, Show)

initialize wWidth wHeight bmWidth bmHeight texFile texUnit = do
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
    
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = 5*sizeOf (0::GLfloat)
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ 4*vertexSize) nullPtr GL_DYNAMIC_DRAW
    
    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf (0::GLfloat))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray ([0,1,2,2,1,3] :: [GLubyte]) $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ 6*sizeOf (0::GLubyte)) ptr GL_STATIC_DRAW
    
    (success,texture) <- Texture.initialize texFile texUnit True
    
    glBindVertexArray 0
    
    return (success, Just $ Bitmap vertexArray vertexBuffer indexBuffer 4 6 texture (wWidth,wHeight) (bmWidth,bmHeight) (-1,-1))
    
    where
    bufferOffset = plusPtr nullPtr . fromIntegral

instance Render Bitmap where
    render bitmap = do
        glBindVertexArray (getVertexArray bitmap)
        
        glDrawElements GL_TRIANGLES (getIndexCount bitmap) GL_UNSIGNED_BYTE nullPtr
        
        glBindVertexArray 0
        
        return (True,bitmap)

instance Update Bitmap where
    update bitmap (II posx posy)
        | (fromIntegral posx,fromIntegral posy) == getPrevPos bitmap = return (True,bitmap)
        | otherwise = do
            let (ww,wh) = getWindowSize bitmap
                (bmw,bmh) = getBitmapSize bitmap
                left = fromIntegral posx - fromIntegral ww/2
                right = left + fromIntegral bmw
                top = fromIntegral wh/2 - fromIntegral posy
                bottom = top - fromIntegral bmh
                vertices :: [GLfloat]
                vertices = [
                    left, top, 0, 0, 1,
                    left, bottom, 0, 0, 0,
                    right, top, 0, 1, 1,
                    right, bottom, 0, 1, 0 ]
                vertexsz = fromIntegral $ 5*sizeOf (0::GLfloat)
            glBindVertexArray (getVertexArray bitmap)
    
            glBindBuffer GL_ARRAY_BUFFER (getVertexBuffer bitmap)
            
            withArray vertices $
                flip (glBufferData GL_ARRAY_BUFFER (4*vertexsz)) GL_DYNAMIC_DRAW
            
            return (True,bitmap { getPrevPos = (fromIntegral posx, fromIntegral posy) })

    update bitmap _ = do
        putStrLn "Incorrect bitmap parameters."
        return (False,bitmap)

instance Shutdown Bitmap where
    shutdown (Bitmap vArray vBuffer iBuffer _ _ (Just texture) _ _ _) = do
        shutdown texture
        
        glBindVertexArray vArray
        
        glDisableVertexAttribArray 0
        glDisableVertexAttribArray 1
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with vArray $ glDeleteVertexArrays 1

bitmapTexture = maybe 0 textureUnit . getTexture
