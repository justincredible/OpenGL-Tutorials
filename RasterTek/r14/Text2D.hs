module Text2D (Text2D,Text2DExt(..),Text2D.initialize) where

import Control.Monad
import Data.Foldable
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import Graphics.UI.GLFW

import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
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

data Text2DExt = Text Text2D [GLfloat] [GLfloat]

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
        vertexBuffer 0 --(4*maxlen)
        indexBuffer 0 --(6*maxlen)
        maxlen
        [0,0,0,1]

-- not exported from module, so Update is not instanced
updateSentence sentence vertices count r g b = do
    let colour = [r,g,b,1]
    if count > getMaxLength sentence
    then return sentence { getColour = colour }
    else do
        glBindBuffer GL_ARRAY_BUFFER (getVertexBuffer sentence)
        
        withArray vertices $
            glBufferSubData GL_ARRAY_BUFFER 0 (fromIntegral $ 4*20*count)
            
        glBindBuffer GL_ARRAY_BUFFER 0
        
        return sentence {
            getVertexCount = 4*count,
            getIndexCount = 6*count,
            getColour = colour }

instance Render Sentence where
    render sentence = do
        glBindVertexArray (getVertexArray sentence)
        
        glDrawElements GL_TRIANGLES (fromIntegral . getIndexCount $ sentence) GL_UNSIGNED_BYTE nullPtr
        
        glBindVertexArray 0
        
        return (True,sentence)

instance Shutdown Sentence where
    shutdown sentence = do
        glBindVertexArray (getVertexArray sentence)
        
        sequence_ $ map glDisableVertexAttribArray [0..1]
        
        with (getVertexBuffer sentence) $ glDeleteBuffers 1
        with (getIndexBuffer sentence) $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with (getVertexArray sentence) $ glDeleteVertexArrays 1

initialize width height baseview = do
    (scsfont,mfont) <- Font.initialize "asset/fontdata.txt" "asset/font.tga" 0
    (scsshader,shader) <- FontShader.initialize
    
    maybe
        (maybe (pure ()) shutdown shader >> return (False,Nothing))
        (\font -> do
            let [(c1,d1),(c2,d2)] = sentenceData width height "Hello" 100 100 "Goodbye" 100 200 font
            s1 <- initSentence 16 >>= \s -> updateSentence s d1 c1 1 1 1
            s2 <- initSentence 16 >>= \s -> updateSentence s d2 c2 1 1 0
            return (scsfont && scsshader, Just $ Text2D mfont shader (width,height) baseview s1 s2))
        mfont

sentenceData width height text1 posx1 posy1 text2 posx2 posy2 font = [
    buildVertexArray 0 font id text1 (posx1 - fromIntegral width/2) (fromIntegral height/2 - posy1),
    buildVertexArray 0 font id text2 (posx2 - fromIntegral width/2) (fromIntegral height/2 - posy2) ]

instance Render Text2DExt where
    render arg@(Text (Text2D _ (Just shader) _ baseview s1 s2) world ortho) = do
        parameters shader world baseview ortho 0 (getColour s1)
        render s1
        
        parameters shader world baseview ortho 0 (getColour s2)
        render s2
        
        return (True,arg)

instance Update Text2D where
    update text2D (II mx my) = do
        let (width,height) = getScreenSize text2D
        
        maybe
            (return (False,text2D))
            (\font -> do
                let [(c1,d1),(c2,d2)] = sentenceData width height ("Mouse X: " ++ show mx) 20 20 ("Mouse Y: " ++ show my) 20 40 font
                s1 <- updateSentence (getSentence1 text2D) d1 c1 1 1 1
                s2 <- updateSentence (getSentence2 text2D) d2 c2 1 1 1
                
                return . (,) True $ text2D {
                    getSentence1 = s1 ,
                    getSentence2 = s2 })
            (getFont text2D)
    update text2D _ = do
        putStrLn "Incorrect text2D parameters."
        return (False,text2D)

instance Shutdown Text2D where
    shutdown (Text2D font shader _ _ s1 s2) = do
        shutdown s1
        shutdown s2
        
        shutdown shader
        shutdown font
