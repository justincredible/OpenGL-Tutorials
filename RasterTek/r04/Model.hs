module Model (Model,Model.initialize) where

import Graphics.GL
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flow.Render
import Flow.Shutdown

data Model = Model {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei }
    deriving (Eq, Show)

initialize texFile texUnit wrap = do
    let vertices = [
            -1, -1, 0,    0, 0, 1,
            1, -1, 0,     1, 0, 0,
            0, 1, 0,      0, 1, 0 ] :: [GLfloat]
        numVertices = length vertices
        indices = [0..2] :: [GLuint]
        numIndices = length indices
    
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
        
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek

    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = 6*sizeOf (head vertices)
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVertices*vertexSize) ptr GL_STATIC_DRAW

    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf (0::GLfloat))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    return . (,) True . Just $ Model vertexArray vertexBuffer indexBuffer (fromIntegral numIndices)

    where
    bufferOffset = plusPtr nullPtr . fromIntegral

instance Render Model where
    render model@(Model vArray _ _ iCount ) = do
        glBindVertexArray vArray
        
        glDrawElements GL_TRIANGLES iCount GL_UNSIGNED_INT nullPtr
        
        return (True,model)

instance Shutdown Model where
    shutdown (Model vArray vBuffer iBuffer _) = do
        glBindVertexArray vArray
        
        glDisableVertexAttribArray 0
        glDisableVertexAttribArray 1
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with vArray $ glDeleteVertexArrays 1
