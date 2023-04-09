module Texture (loadDDS,loadBMP_demo) where

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

loadBMP_demo file = do
    (header,filedat) <- fmap (BS.splitAt 54) $ BS.readFile file
    
    let shortHeader = BS.length header /= 54
        wrongType = (w2c . BS.head) header /= 'B' || (w2c . BS.head . BS.tail) header /= 'M'
        wrongBPP = (bs2i . subytestr 28 4) header /= 24 || (bs2i . subytestr 30 4) header /= 0

    if shortHeader || wrongType || wrongBPP
    then putStrLn "Not a correct BMP file" >> return 0
    else do
        let datapos' = bs2i . subytestr 10 4 $ header
            width = bs2i . subytestr 18 4 $ header
            height = bs2i . subytestr 22 4 $ header
            imagesize' = bs2i . subytestr 34 4 $ header
            datapos = if datapos' == 0 then 54 else datapos'
            imagesize = if imagesize' == 0 then width*height*3 else imagesize'
        
        textureID <- alloca $ \ptr -> do
            glGenTextures 1 ptr
            peek ptr
        
        glBindTexture GL_TEXTURE_2D textureID
        
        let rgb = fromIntegral GL_RGB
            wrap = fromIntegral GL_REPEAT
            magfilter = fromIntegral GL_LINEAR 
            minfilter = fromIntegral GL_LINEAR_MIPMAP_LINEAR
        
        withArray (BS.unpack filedat) $
            glTexImage2D GL_TEXTURE_2D 0 rgb width height 0 GL_BGR GL_UNSIGNED_BYTE
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S wrap
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T wrap
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER magfilter
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER minfilter
        glGenerateMipmap GL_TEXTURE_2D
        
        return textureID
    where
    subytestr d t = BS.unpack . BS.take t . BS.drop d
    bs2i = foldr ((. (*256)).(+).fromIntegral) 0
