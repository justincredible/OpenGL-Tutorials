module Model (Model,Model.initialize,modelTexture,getPosition) where

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

data Model = Model {
    getPosition :: [GLfloat],
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei,
    getTexture :: Maybe Texture }
    deriving (Eq, Show)

initialize dataFile texFile texUnit wrap = do
    (numVertices,vertices) <- fmap read . readFile $ dataFile :: IO (Int,[GLfloat])
    let indices = [0..fromIntegral numVertices - 1] :: [GLuint]
        numIndices = numVertices
    
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
        
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek

    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = sizeOf (head vertices)*quot (length vertices) numVertices
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVertices*vertexSize) ptr GL_STATIC_DRAW

    sequence_ $ map glEnableVertexAttribArray [0..2]
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf (0::GLfloat))
    glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (5*sizeOf (0::GLfloat))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    (success, texture) <- Texture.initialize texFile texUnit wrap
    
    glBindVertexArray 0
    
    return (success, Just $ Model [0,0,0] vertexArray vertexBuffer indexBuffer (fromIntegral numIndices) texture)

    where
    bufferOffset = plusPtr nullPtr . fromIntegral

instance Render Model where
    render model = do
        glBindVertexArray (getVertexArray model)
        
        glDrawElements GL_TRIANGLES (getIndexCount model) GL_UNSIGNED_INT nullPtr
        
        glBindVertexArray 0
        
        return (True,model)

instance Shutdown Model where
    shutdown model = do
        shutdown (getTexture model)
        
        glBindVertexArray (getVertexArray model)
        
        sequence_ $ map glDisableVertexAttribArray [0..2]
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with (getIndexBuffer model) $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with (getVertexBuffer model) $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with (getVertexArray model) $ glDeleteVertexArrays 1

modelTexture :: Model -> GLint
modelTexture = fromIntegral . maybe 0 textureUnit . getTexture
