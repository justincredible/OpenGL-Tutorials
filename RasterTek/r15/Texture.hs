module Texture (Texture,initialize,getTextureID,getTextureUnit) where

import Prelude
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import Graphics.GL

import Flow.Shutdown

data Texture = Texture { getTextureID :: GLuint, getTextureUnit :: GLint }
    deriving (Eq, Show)

-- load Targa (32,4) texture
initialize texFile texUnit wrap = do
    bytes <- BS.readFile texFile
    let width = BS.foldr horner 0 . BS.take 2 . BS.drop 12 $ bytes
        height = BS.foldr horner 0 . BS.take 2 . BS.drop 14 $ bytes
        bpp = BS.head . BS.drop 16 $ bytes
        image = BS.unpack . BS.take (width*height*4) . BS.drop 18 $ bytes
    
    if (bpp /= 32)
    then do
        putStrLn "loadTarga requires 32 bits per pixel."
        return (False, Nothing)
    else do
        glActiveTexture $ GL_TEXTURE0 + texUnit
        
        texID <- alloca $ (>>) . glGenTextures 1 <*> peek
        
        glBindTexture GL_TEXTURE_2D texID
        
        withArray image $ \ptr ->
            glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) (fromIntegral width) (fromIntegral height) 0 GL_BGRA GL_UNSIGNED_BYTE ptr
        
        let wrapType = fromIntegral $ if wrap then GL_REPEAT else GL_CLAMP
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S wrapType
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T wrapType
        
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
        
        glGenerateMipmap GL_TEXTURE_2D
        
        return . (,) True . Just $ Texture texID (fromIntegral texUnit)
    where
    horner w i = i*2^finiteBitSize w + fromIntegral w

instance Shutdown Texture where
    shutdown (Texture texID _) = with texID $ glDeleteTextures 1
