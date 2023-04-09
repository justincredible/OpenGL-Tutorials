module Text2D (Text2D,Text2DArg(..),Text2D.initialize) where

import Control.Monad
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import Font
import FontShader

data Text2D = Text2D {
    getFont :: Maybe Font,
    getFontShader :: Maybe FontShader,
    getScreenSize :: (Int,Int),
    getBaseView :: [GLfloat],
    getSentence1 :: Sentence,
    getSentence2 :: Sentence }
    deriving (Eq,Show)

data Text2DArg = TextArg Text2D [GLfloat] [GLfloat]

data Sentence = Sentence {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getVertexCount :: Int,
    getIndexBuffer :: GLuint,
    getIndexCount :: Int,
    getMaxLength :: Int,
    getColour :: [GLfloat] }
    deriving (Eq,Show)

initSentence maxlen = do
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
    glBindVertexArray vertexArray

    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ 4*20*maxlen) nullPtr GL_DYNAMIC_DRAW
    
    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 20 nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 20 $ (plusPtr nullPtr . fromIntegral) 12
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray (buildIndexArray (replicate maxlen ' ')) $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ 6*maxlen) ptr GL_STATIC_DRAW

    glBindVertexArray 0
    
    return $ Sentence vertexArray
        vertexBuffer (4*maxlen)
        indexBuffer (6*maxlen)
        maxlen
        [0,0,0,1]
    
updateSentence sentence font text scrw scrh posx posy r g b = do
    if length text > getMaxLength sentence
    then return sentence { getColour = [r,g,b,1] }
    else do
        let ccount = length text
        
        glBindVertexArray (getVertexArray sentence)
        
        withArray (buildVertexArray font text (posx - fromIntegral scrw/2) (fromIntegral scrh/2 - posy)) $
            glBufferSubData GL_ARRAY_BUFFER 0 (4*fromIntegral ccount*20)
        
        glBindVertexArray 0
        
        return sentence {
            getVertexCount = 4*ccount,
            getIndexCount = 6*ccount,
            getColour = [r,g,b,1] }

instance Render Sentence where
    render sentence = do
        glBindVertexArray (getVertexArray sentence)
        
        glDrawElements GL_TRIANGLES (fromIntegral . getIndexCount $ sentence) GL_UNSIGNED_BYTE nullPtr
        
        glBindVertexArray 0
        
        return (True,sentence)

instance Shutdown Sentence where
    shutdown sentence = do
        glBindVertexArray (getVertexArray sentence)
        
        glDisableVertexAttribArray 0
        glDisableVertexAttribArray 1
        
        with (getVertexBuffer sentence) $ glDeleteBuffers 1
        with (getIndexBuffer sentence) $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with (getVertexArray sentence) $ glDeleteVertexArrays 1

initialize width height baseview = do
    (scsfont,Just font) <- Font.initialize "asset/fontdata.txt" "asset/font.tga" 0
    (scsshader,shader) <- FontShader.initialize
    s1 <- initSentence 16
    s1' <- updateSentence s1 font "Hello" width height 100 100 1 1 1
    s2 <- initSentence 16
    s2' <- updateSentence s2 font "Goodbye" width height 100 200 1 1 0
    
    return (scsfont && scsshader, Just $ Text2D (Just font) shader (width,height) baseview s1' s2')

instance Render Text2DArg where
    render arg@(TextArg (Text2D _ (Just shader) _ baseview s1 s2) world ortho) = do
        parameters shader world baseview ortho 0 (getColour s1)
        render s1
        
        parameters shader world baseview ortho 0 (getColour s2)
        render s2
        
        return (True,arg)

instance Shutdown Text2D where
    shutdown (Text2D font shader _ _ s1 s2) = do
        shutdown s1
        shutdown s2
        
        shutdown shader
        shutdown font
