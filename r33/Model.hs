module Model (Model,Model.initialize,modelTexture,noiseTexture,alphaTexture) where

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
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei,
    getTexture :: Maybe Texture,
    getNoise :: Maybe Texture,
    getAlpha :: Maybe Texture }
    deriving (Eq, Show)

initialize dataFile clrTex clrUnit clrWrap noiTex noiUnit noiWrap alfTex alfUnit alfWrap = do
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

    sequence_ $ map glEnableVertexAttribArray [0..1]
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf (0::GLfloat))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    (success1, clrtex) <- Texture.initialize clrTex clrUnit clrWrap
    (success2, noitex) <- Texture.initialize noiTex noiUnit noiWrap
    (success3, alftex) <- Texture.initialize alfTex alfUnit alfWrap
    
    glBindVertexArray 0
    
    return (success1 && success2 && success3, Just $ Model vertexArray vertexBuffer indexBuffer (fromIntegral numIndices) clrtex noitex alftex)

    where
    bufferOffset = plusPtr nullPtr . fromIntegral

instance Render Model where
    render model = do
        glBindVertexArray (getVertexArray model)
        
        glDrawElements GL_TRIANGLES (getIndexCount model) GL_UNSIGNED_INT nullPtr
        
        glBindVertexArray 0
        
        return (True,model)

instance Shutdown Model where
    shutdown (Model vArray vBuffer iBuffer _ tex1 tex2 tex3) = do
        shutdown tex1
        
        shutdown tex2
        
        shutdown tex3
        
        glBindVertexArray vArray
        
        sequence_ $ map glDisableVertexAttribArray [0..1]
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with vArray $ glDeleteVertexArrays 1

modelTexture (Model _ _ _ _ (Just texture) _ _) = fromIntegral . textureUnit $ texture
noiseTexture (Model _ _ _ _ _ (Just texture) _) = fromIntegral . textureUnit $ texture
alphaTexture (Model _ _ _ _ _ _ (Just texture)) = fromIntegral . textureUnit $ texture
