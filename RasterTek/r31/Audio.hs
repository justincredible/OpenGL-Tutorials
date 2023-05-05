module Audio (Audio,initialize) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Sound.OpenAL.FFI.AL
import Sound.OpenAL.FFI.ALC

import Flow.Shutdown

data Audio = Audio {
    getDevice :: Device,
    getContext :: Context,
    getBuffer :: Buffer,
    getSource :: Source }
    deriving (Eq,Show)

--https://indiegamedev.net/2020/02/15/the-complete-guide-to-openal-with-c-part-1-playing-a-sound/
initialize file = do
    device <- alcOpenDevice nullPtr
    context <- alcCreateContext device nullPtr
    alcMakeContextCurrent context
    
    (success,audata,(format,freq,size)) <- loadWave file
    buffer <- alloca $ (>>) . alGenBuffers 1 <*> peek
    withArray audata $ \ptr -> alBufferData buffer format (castPtr ptr) size freq
    source <- alloca $ (>>) . alGenSources 1 <*> peek
    
    withArray [1] $ alSourcefv source PITCH
    withArray [1] $ alSourcefv source GAIN
    withArray [-2,0,0] $ alSourcefv source POSITION
    withArray [0,0,0] $ alSourcefv source VELOCITY
    alSourcei source LOOPING 0
    let (Buffer bufId) = buffer
    alSourcei source BUFFER (fromIntegral bufId)
    
    with source $ alSourcePlayv 1
    
    return (True, Audio device context buffer source)

instance Shutdown Audio where
    shutdown audio = do
        state <- alloca $ (>>) . alGetSourceiv (getSource audio) SOURCE_STATE <*> peek
        when (state == PLAYING) $ with (getSource audio) (alSourceStopv 1)
        
        with (getSource audio) $ alDeleteSources 1
        with (getBuffer audio) $ alDeleteBuffers 1
        alcMakeContextCurrent (Context nullPtr)
        alcDestroyContext (getContext audio)
        alcCloseDevice (getDevice audio)
        return ()

loadWaveHeader header
    | BS.length header /= 44 = putStrLn "Not enough header data." >> return (False,(0,0,0))
    | (toString . BS.take 4) header /= "RIFF" ||
        (toString . BS.take 4 . BS.drop 8) header /= "WAVE" ||
        (toString . BS.take 4 . BS.drop 36) header /= "data" = putStrLn "Not a valid WAVE file." >> return (False,(0,0,0))
    | otherwise = do
        let samplerate = readIntegral 4 24 header
            size = readIntegral 4 40 header
            format = case (readIntegral 2 22 header,readIntegral 2 34 header) of
                (1,8) -> FORMAT_MONO8
                (1,16) -> FORMAT_MONO16
                (2,8) -> FORMAT_STEREO8
                (2,16) -> FORMAT_STEREO16
                _ -> undefined
            
        return (True,(format,fromIntegral samplerate,fromIntegral size))
    where
    readIntegral size offset = foldr (\a b -> a + b*256) 0
        . map fromEnum
        . BS.unpack
        . BS.take size
        . BS.drop offset
    toString = map (toEnum . fromEnum) . BS.unpack

loadWave file = do
    (header,contents) <- fmap (BS.splitAt 44) . BS.readFile $ file
    (result,info) <- loadWaveHeader header
    return (result,BS.unpack contents,info)
