module InstanceModel (InstanceModel,InstanceModel.initialize,modelTexture) where

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
import Texture

data InstanceModel = InstanceModel {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getInstanceBuffer :: GLuint,
    getInstanceCount :: GLsizei,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei,
    getTexture :: Maybe Texture }
    deriving (Eq, Show)

initialize texFile texUnit wrap = do
    let numIndices = 3
        numVertices = 3
        numInstances = 4
        indices = [0..2] :: [GLubyte]
        vertices = [
            -1,-1,0,0,1,
            1,-1,0,1,1,
            0,1,0,0.5,0 ] :: [GLfloat]
        instances = [
            -1.5,-1.5,5,
            -1.5,1.5,5,
            1.5,-1.5,5,
            1.5,1.5,5 ] :: [GLfloat]
        
    -- using a VAO and element buffer is unnecessary for this data, but may be useful for more complicated models
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
        
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek

    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = sizeOf (head vertices)*quot (length vertices) numVertices
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVertices*vertexSize) ptr GL_STATIC_DRAW

    sequence_ $ map glEnableVertexAttribArray [0..2]
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf(GL_FLOAT))
    
    instanceBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek

    glBindBuffer GL_ARRAY_BUFFER instanceBuffer
    
    let instanceSize = sizeOf (head instances)*quot (length instances) numInstances
    withArray instances $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numInstances*instanceSize) ptr GL_STATIC_DRAW
    
    glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE (fromIntegral instanceSize) nullPtr

    glVertexAttribDivisor 0 0
    glVertexAttribDivisor 1 0
    glVertexAttribDivisor 2 1
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    (success, texture) <- Texture.initialize texFile texUnit wrap
    
    glBindVertexArray 0
    
    return (success, Just $ InstanceModel vertexArray vertexBuffer instanceBuffer (fromIntegral numInstances) indexBuffer (fromIntegral numIndices) texture)

    where
    bufferOffset = plusPtr nullPtr . fromIntegral

instance Render InstanceModel where
    render model = do
        glBindVertexArray (getVertexArray model)
        
        glDrawElementsInstanced GL_TRIANGLES (getIndexCount model) GL_UNSIGNED_BYTE nullPtr (getInstanceCount model)
        
        glBindVertexArray 0
        
        return (True,model)

instance Shutdown InstanceModel where
    shutdown (InstanceModel vArray vBuffer nBuffer _ iBuffer _ texture) = do
        shutdown texture
        
        glBindVertexArray vArray
        
        sequence_ $ map glDisableVertexAttribArray [0..2]
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ glDeleteBuffers 1
        with nBuffer $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with vArray $ glDeleteVertexArrays 1

modelTexture (InstanceModel _ _ _ _ _ _ (Just texture)) = fromIntegral . textureUnit $ texture
