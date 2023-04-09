module Text2D (initText2D, printText2D, cleanupText2D) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

import ShaderCompilinker
import Texture

data Text2D = Text2D {
    getTexture :: GLuint,
    getVbuffer :: GLuint,
    getTbuffer :: GLuint,
    getShader :: GLuint,
    getSampler :: GLint
    }
    deriving (Eq, Show)

initText2D file = do
    texture <- loadDDS file
    
    vbuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    tbuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    shader <- compileAndLink "glsl/text.vertex" "glsl/text.fragment"
    
    sampler <- withArray0 0 (map castCharToCChar "font") $ glGetUniformLocation shader
    
    return $ Text2D texture vbuffer tbuffer shader sampler


printText2D (Text2D tid vid cid pid uid) text x y size = do
    let (positions, coordinates) = transform text
    
    glBindBuffer GL_ARRAY_BUFFER vid
    withArray (concat positions) $ \ptr -> glBufferData GL_ARRAY_BUFFER (ecs positions) ptr GL_STATIC_DRAW
    glBindBuffer GL_ARRAY_BUFFER cid
    withArray (concat coordinates) $ \ptr -> glBufferData GL_ARRAY_BUFFER (ecs coordinates) ptr GL_STATIC_DRAW
    
    glUseProgram pid
    
    glActiveTexture GL_TEXTURE0;
    glBindTexture GL_TEXTURE_2D tid
    glUniform1i uid 0

    glEnableVertexAttribArray 0
    glBindBuffer GL_ARRAY_BUFFER vid
    glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 0 nullPtr

    glEnableVertexAttribArray 1
    glBindBuffer GL_ARRAY_BUFFER cid
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 0 nullPtr

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    glDrawArrays GL_TRIANGLES  0 . fromIntegral . length $ positions

    glDisable GL_BLEND

    glDisableVertexAttribArray 0
    glDisableVertexAttribArray 1
    where
    transform :: [Char] -> ([[GLfloat]],[[GLfloat]])
    transform text = f 0 text id id 
    f i [] dp dc = (dp [], dc [])
    f i (c:cs) dp dc = let
        vul = [x + i*size, y + size]
        vur = [x + i*size + size, y + size]
        vdr = [x + i*size + size, y]
        vdl = [x + i*size, y]
        c' = fromEnum c
        u = fromIntegral (rem c' 16)/16.0
        v = fromIntegral (quot c' 16)/16.0
        tul = [u, v]
        tur = [u + 1/16, v]
        tdr = [u + 1/16, v + 1/16]
        tdl = [u, v + 1/16]
        in
        f (i+1) cs
            (dp.(vul:).(vdl:).(vur:).(vdr:).(vur:).(vdl:))
            (dc.(tul:).(tdl:).(tur:).(tdr:).(tur:).(tdl:))

    -- elements*components*size
    ecs :: Storable a => [[a]] -> CPtrdiff
    ecs = fromIntegral . ((*) . length <*> ((*).length.head <*> sizeOf.head.head))


cleanupText2D (Text2D tid vid cid pid _) = do
    sequence_ $ zipWith cleanup
        [vid, cid, tid]
        [glDeleteBuffers, glDeleteBuffers, glDeleteTextures]
    
    glDeleteProgram pid
    where
    cleanup id f = with id $ f 1
