module OrthoWindow (OrthoWindow,initialize) where

import Graphics.GL
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flow.Render
import Flow.Shutdown
import Maths

data OrthoWindow = OrthoWindow {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei }
    deriving (Eq, Show)

initialize width height = do
    let left = -fromIntegral width/2
        right = left + fromIntegral width
        top = fromIntegral height/2
        bottom = top - fromIntegral height
        numIndices = 6
        indices = [0,1,2,2,1,3] :: [GLubyte]
        numVertices = 4
        vertices = [
            left,top,0,0,1,
            left,bottom,0,0,0,
            right,top,0,1,1,
            right,bottom,0,1,0 ] :: [GLfloat]
    
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
        
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek

    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = sizeOf (head vertices)*quot (length vertices) numVertices
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVertices*vertexSize) ptr GL_STATIC_DRAW

    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf(GL_FLOAT))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    glBindVertexArray 0
    
    return (True, Just $ OrthoWindow vertexArray vertexBuffer indexBuffer (fromIntegral numIndices))

    where
    bufferOffset = plusPtr nullPtr . fromIntegral

instance Render OrthoWindow where
    render orthoWindow = do
        glBindVertexArray (getVertexArray orthoWindow)
        
        glDrawElements GL_TRIANGLES (getIndexCount orthoWindow) GL_UNSIGNED_BYTE nullPtr
        
        glBindVertexArray 0
        
        return (True,orthoWindow)

instance Shutdown OrthoWindow where
    shutdown (OrthoWindow vArray vBuffer iBuffer _) = do
        glBindVertexArray vArray
        
        glDisableVertexAttribArray 0
        glDisableVertexAttribArray 1
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with vArray $ glDeleteVertexArrays 1
