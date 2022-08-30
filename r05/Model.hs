module Model (Model,Model.initialize,getTextureUnit) where

import Graphics.GL
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flow.Render
import Flow.Shutdown
import Texture

data Model = Model {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei,
    getTexture :: Maybe Texture }
    deriving (Eq, Show)

initialize texFile texUnit wrap = do
    let vertices = [
            0, 1, 0,      0.5, 1,
            1, -1, 0,     1, 0,
            -1, -1, 0,    0, 0 ]  :: [GLfloat]
        {-vertices = [
            0, 1, 0,      1, 0, 0,
            1, -1, 0,     0, 1, 0,
            -1, -1, 0,    0, 0, 1 ]  :: [GLfloat]-}
        numVertices = length vertices
        indices = [0..2] :: [GLuint]
        numIndices = length indices
    
    vertexArray <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
        
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr

    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = 5*sizeOf (head vertices)
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVertices*vertexSize) ptr GL_STATIC_DRAW

    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf(GL_FLOAT))
    
    indexBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    (success, texture) <- Texture.initialize texFile texUnit wrap
    
    return (success, Just $ Model vertexArray vertexBuffer indexBuffer (fromIntegral numIndices) texture)

    where
    bufferOffset = plusPtr nullPtr . fromIntegral

instance Render Model where
    render model@(Model vArray _ _ iCount _) = do
        glBindVertexArray vArray
        
        glDrawElements GL_TRIANGLES iCount GL_UNSIGNED_INT nullPtr
        
        return (True,model)

instance Shutdown Model where
    shutdown (Model vArray vBuffer iBuffer _ texture) = do
        shutdown texture
        
        glBindVertexArray vArray
        
        glDisableVertexAttribArray 0
        glDisableVertexAttribArray 1
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ \ptr ->
            glDeleteBuffers 1 ptr
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ \ptr ->
            glDeleteBuffers 1 ptr
        
        glBindVertexArray 0
        with vArray $ \ptr ->
            glDeleteVertexArrays 1 ptr

getTextureUnit (Model _ _ _ _ (Just texture)) = fromIntegral . getTextureID $ texture
