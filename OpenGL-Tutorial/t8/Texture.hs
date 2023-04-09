module Texture (loadDDS) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import qualified Data.ByteString.Unsafe as BSU
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

loadDDS file = do
    (fileType,(header,compressed)) <- fmap (fmap (BS.splitAt 124).BS.splitAt 4) $
        BS.readFile file
    
    if fileType /= (BS.pack . map c2w) "DDS " || BS.length header /= 124
    then return 0  
    else do
        let height = readWord32 . BS.take 4 . BS.drop 8 $ header
            width = readWord32 . BS.take 4 . BS.drop 12 $ header
            linearSize = readWord32 . BS.take 4 . BS.drop 16 $ header
            mipMapCount = readWord32 . BS.take 4 . BS.drop 24 $ header
            fourCC = BS.take 4 . BS.drop 80 $ header
            bufsize = (if mipMapCount > 1 then (*2) else id) linearSize
            buffer = BS.take (fromIntegral bufsize) compressed
            (components,format,blocksize) = formatValues fourCC
            offset = 0
        
        textureID <- alloca $ \ptr -> do
            glGenTextures 1 ptr
            peek ptr
        
        glBindTexture GL_TEXTURE_2D textureID
        
        withArray (BS.unpack compressed) $ \ptr ->
            loadMipMaps (fromIntegral mipMapCount) format blocksize ptr (fromIntegral width) (fromIntegral height) 0

        return textureID

    where
    readWord32 :: ByteString -> Word32
    readWord32 = BS.foldr (\a b -> b*2^finiteBitSize a + fromIntegral a) 0
    formatValues fourCC
        | fourCC == (BS.pack . map c2w) "DXT1" = (3,GL_COMPRESSED_RGB_S3TC_DXT1_EXT,8)
        | fourCC == (BS.pack . map c2w) "DXT3" = (4,GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,16)
        | fourCC == (BS.pack . map c2w) "DXT5" = (4,GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,16)
        | otherwise = (4,0,16)
    loadMipMaps mmcount format blksz ptr width height level =
        when (mmcount /= level) $ do
            let size = (quot (width+3) 4)*(quot (height+3) 4)*blksz
            glCompressedTexImage2D GL_TEXTURE_2D level format width height 0 size ptr
            loadMipMaps mmcount format blksz (plusPtr ptr $ fromIntegral size) (quot width 2) (quot height 2) (level + 1)
