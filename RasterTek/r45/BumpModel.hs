module BumpModel (BumpModel,BumpModel.initialize,bumpModelTexture,bumpModelNormal) where

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

data BumpModel = BumpModel {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei,
    getTexture :: Maybe Texture,
    getNormal :: Maybe Texture }
    deriving (Eq, Show)

initialize dataFile texFile texUnit wrap nrmFile nrmUnit nrmWrap = do
    (numVertices,verts) <- fmap read . readFile $ dataFile :: IO (Int,[GLfloat])
    let indices = [0..fromIntegral numVertices - 1] :: [GLuint]
        numIndices = numVertices
    
    let vertices = concat
            . calculateBumpModelVectors
            . unconcat (quot (length verts) numVertices)
            $ verts
    
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
        
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek

    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = sizeOf (head vertices)*quot (length vertices) numVertices
    withArray vertices $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVertices*vertexSize) ptr GL_STATIC_DRAW

    sequence_ $ map glEnableVertexAttribArray [0..4]
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf (0::GLfloat))
    glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (5*sizeOf (0::GLfloat))
    glVertexAttribPointer 3 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (8*sizeOf (0::GLfloat))
    glVertexAttribPointer 4 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (11*sizeOf (0::GLfloat))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    (success1, texture) <- Texture.initialize texFile texUnit wrap
    (success2, normal) <- Texture.initialize nrmFile nrmUnit nrmWrap
    
    glBindVertexArray 0
    
    return (success1 && success2, Just $ BumpModel vertexArray vertexBuffer indexBuffer (fromIntegral numIndices) texture normal)

    where
    bufferOffset = plusPtr nullPtr . fromIntegral
    length = foldr ((+).const 1) 0
    unconcat _ [] = []
    unconcat n xs = ((:) . fst <*> unconcat n . snd) . splitAt n $ xs

instance Render BumpModel where
    render bumpModel@(BumpModel vArray _ _ iCount _ _) = do
        glBindVertexArray vArray
        
        glDrawElements GL_TRIANGLES iCount GL_UNSIGNED_INT nullPtr
        
        glBindVertexArray 0
        
        return (True,bumpModel)

instance Shutdown BumpModel where
    shutdown (BumpModel vArray vBuffer iBuffer _ texture normal) = do
        shutdown texture
        shutdown normal
        
        glBindVertexArray vArray
        
        sequence_ $ map glDisableVertexAttribArray [0..4]
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with vArray $ glDeleteVertexArrays 1

calculateBumpModelVectors (a:b:c:vertices) = let
    (tangent,bitangent) = calculateTangentBitangent
    newnormal = calculateNormal tangent bitangent
    in
    extend (newnormal++tangent++bitangent) (calculateBumpModelVectors vertices)
    where
    extend extension continuation =
        (take 5 a ++ extension):
        (take 5 b ++ extension):
        (take 5 c ++ extension):
        continuation
    calculateNormal = (normalize .) . flip cross
    calculateTangentBitangent = let
        [x1,y1,z1] = minus (take 3 b) (take 3 a)
        [x2,y2,z2] = minus (take 3 c) (take 3 a)
        [u1,v1] = take 2 . drop 3 $ a
        [u2,v2] = take 2 . drop 3 $ b
        [u3,v3] = take 2 . drop 3 $ c
        den = 1/((u2-u1)*(v3-v1) - (u3-u1)*(v2-v1))
        in
        (normalize . map (*den) $ [(v3-v1)*x1-(v2-v1)*x2,(v3-v1)*y1-(v2-v1)*y2,(v3-v1)*z1-(v2-v1)*z2]
        ,normalize . map (*den) $ [(u2-u1)*x2-(u3-u1)*x1,(u2-u1)*y2-(u3-u1)*y1,(u2-u1)*z2-(u3-u1)*z1])
calculateBumpModelVectors _ = []

bumpModelTexture (BumpModel _ _ _ _ (Just texture) _) = fromIntegral . textureUnit $ texture
bumpModelNormal (BumpModel _ _ _ _ _ (Just texture)) = fromIntegral . textureUnit $ texture
