module TextureArray (TextureArray,initialize,texarrUnit) where

import Control.Arrow
import Data.Bits
import Data.List
import qualified Data.ByteString as BS
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Graphics.GL

import Flow.Shutdown

data TextureArray = TextureArray {
    getID :: GLuint,
    getUnit :: GLint,
    getDepth :: Int }
    deriving (Eq, Show)

-- load Targa (32,4) textures
initialize _ [] _ = return (False,Nothing)
initialize texUnit texFiles wrap = do
    let numTexas = length texFiles
    
    textures <- sequence $ map parse texFiles
    
    if (length . nub . map fst) textures /= 1
        || (fst . snd . head) textures /= 32
    then do
        putStrLn "TextureArray requires uniform texture dimensions and 32 bits per pixel."
        return (False, Nothing)
    else do
        glActiveTexture $ GL_TEXTURE0 + fromIntegral texUnit
        
        texID <- alloca $ (>>) . glGenTextures 1 <*> peek
        
        glBindTexture GL_TEXTURE_2D_ARRAY texID
        
        let (width,height) = fst . head $ textures
        
        withArray (concat . map (snd.snd) $ textures) $
            glTexImage3D GL_TEXTURE_2D_ARRAY 0 (fromIntegral GL_RGBA) (fromIntegral width) (fromIntegral height) (fromIntegral numTexas) 0 GL_BGRA GL_UNSIGNED_BYTE
        
        let wrapType = fromIntegral $ if wrap then GL_REPEAT else GL_CLAMP
        glTexParameteri GL_TEXTURE_2D_ARRAY GL_TEXTURE_WRAP_S wrapType
        glTexParameteri GL_TEXTURE_2D_ARRAY GL_TEXTURE_WRAP_T wrapType
        
        glTexParameteri GL_TEXTURE_2D_ARRAY GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
        glTexParameteri GL_TEXTURE_2D_ARRAY GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR)
        
        return . (,) True . Just $ TextureArray texID texUnit numTexas
    where
    horner w i = i*2^finiteBitSize w + fromIntegral w
    parse texFile = do
        bytes <- BS.readFile texFile
        let width = BS.foldr horner 0 . BS.take 2 . BS.drop 12 $ bytes
            height = BS.foldr horner 0 . BS.take 2 . BS.drop 14 $ bytes
            bpp = BS.head . BS.drop 16 $ bytes
            image = BS.unpack . BS.take (width*height*4) . BS.drop 18 $ bytes
        return ((width,height),(bpp,image))

instance Shutdown TextureArray where
    shutdown texarray = with (getID texarray) $ glDeleteTextures 1

texarrUnit = getUnit
