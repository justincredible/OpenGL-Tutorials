module Model (Model,Model.initialize,modelTexture) where

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
import TextureArray

data Model = Model {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getIndexCount :: GLsizei,
    getTexture :: Maybe TextureArray }
    deriving (Eq, Show)

initialize dataFile texFile1 texFile2 texUnit wrap = do
    (numVertices,verts) <- fmap read . readFile $ dataFile :: IO (Int,[GLfloat])
    let indices = [0..fromIntegral numVertices - 1] :: [GLuint]
        numIndices = numVertices
    
    let vertices = concat
            . calculateModelVectors
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
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf(GL_FLOAT))
    glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (5*sizeOf(GL_FLOAT))
    glVertexAttribPointer 3 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (8*sizeOf(GL_FLOAT))
    glVertexAttribPointer 4 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (11*sizeOf(GL_FLOAT))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray indices $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ numIndices*sizeOf (head indices)) ptr GL_STATIC_DRAW
    
    (success, texarray) <- TextureArray.initialize texUnit [texFile1,texFile2] wrap
    
    glBindVertexArray 0
    
    return (success, Just $ Model vertexArray vertexBuffer indexBuffer (fromIntegral numIndices) texarray)

    where
    bufferOffset = plusPtr nullPtr . fromIntegral
    length = foldr ((+).const 1) 0
    unconcat _ [] = []
    unconcat n xs = ((:) . fst <*> unconcat n . snd) . splitAt n $ xs

instance Render Model where
    render model@(Model vArray _ _ iCount _) = do
        glBindVertexArray vArray
        
        glDrawElements GL_TRIANGLES iCount GL_UNSIGNED_INT nullPtr
        
        glBindVertexArray 0
        
        return (True,model)

instance Shutdown Model where
    shutdown (Model vArray vBuffer iBuffer _ texarray) = do
        shutdown texarray
        
        glBindVertexArray vArray
        
        sequence_ $ map glDisableVertexAttribArray [0..4]
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with vArray $ glDeleteVertexArrays 1

calculateModelVectors (a:b:c:vertices) = let
    (tangent,bitangent) = calculateTangentBitangent
    newnormal = calculateNormal tangent bitangent
    in
    extend (newnormal++tangent++bitangent) (calculateModelVectors vertices)
    where
    extend extension continuation =
        (take 5 a ++ extension):
        (take 5 b ++ extension):
        (take 5 c ++ extension):
        continuation
    calculateNormal [tx,ty,tz] [bx,by,bz] = normalize [ty*bz-tz*by,tz*bx-tx*bz,tx*by-ty*bx]
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
calculateModelVectors _ = []

modelTexture (Model _ _ _ _ (Just texarray)) = fromIntegral . texarrUnit $ texarray
