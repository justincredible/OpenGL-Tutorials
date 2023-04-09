module Texture (Texture,initialize,getTextureID) where

import Prelude hiding (readFile, take, drop, foldr, head)
import Data.Bits
import Data.ByteString hiding (putStrLn)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Storable
import Graphics.GL

import Flow.Shutdown

data Texture = Texture { getTextureID :: GLuint }
    deriving (Eq, Show)

-- load Targa texture
initialize texFile texUnit wrap = do
    bytes <- readFile texFile
    let width = foldr horner 0 . take 2 . drop 12 $ bytes
        height = foldr horner 0 . take 2 . drop 14 $ bytes
        bpp = head . drop 16 $ bytes
        image = unpack . take (width*height*4) . drop 18 $ bytes
    
    if (bpp /= 32)
    then do
        putStrLn "loadTarga requires 32 bits per pixel."
        return (False, Nothing)
    else do
        glActiveTexture $ GL_TEXTURE0 + texUnit
        
        -- not sure why I can't use alloca here 
        let texID = 0
        with texID $ \ptr -> do
            glGenTextures 1 ptr
        
        glBindTexture GL_TEXTURE_2D texID
        
        withArray image $ \ptr ->
            glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) (fromIntegral width) (fromIntegral height) 0 GL_BGRA GL_UNSIGNED_BYTE ptr
        
        let wrapType = fromIntegral $ if wrap then GL_REPEAT else GL_CLAMP
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S wrapType
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T wrapType
        
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
        
        glGenerateMipmap GL_TEXTURE_2D
        
        return . (,) True . Just $ Texture texID
    where
    horner w i = i*2^finiteBitSize w + fromIntegral w

instance Shutdown Texture where
    shutdown (Texture texID) = with texID $ glDeleteTextures 1
