module Audio (Audio,initialize) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Sound.AL
import Sound.ALC

import Flow.Shutdown

data Audio = Audio {
    getDevice :: Ptr ALCdevice,
    getContext :: Ptr ALvoid,
    getBuffer :: ALuint,
    getSource :: ALuint }
    deriving (Eq,Show)

--https://indiegamedev.net/2020/02/15/the-complete-guide-to-openal-with-c-part-1-playing-a-sound/
initialize file = do
    device <- alcOpenDevice nullPtr
    context <- alcCreateContext device nullPtr
    alcMakeContextCurrent context
    {-withArray [0,0,4] $ alListenerfv al_POSITION
    withArray [0,0,0] $ alListenerfv al_VELOCITY
    withArray [0,0,1,0,1,0] $ alListenerfv al_ORIENTATION-}
    
    (success,audata,(format,freq,size)) <- loadWave file
    buffer <- alloca $ (>>) . alGenBuffers 1 <*> peek
    withArray audata $ \ptr -> alBufferData buffer format ptr size freq
    source <- alloca $ (>>) . alGenSources 1 <*> peek
    
    alSourcef source al_PITCH 1
    alSourcef source al_GAIN 1
    withArray [-2,0,0] $ alSourcefv source al_POSITION
    withArray [0,0,0] $ alSourcefv source al_VELOCITY
    alSourcei source al_LOOPING al_FALSE
    alSourcei source al_BUFFER (fromIntegral buffer)
    
    alSourcePlay source
    
    return (True, Audio device context buffer source)

instance Shutdown Audio where
    shutdown audio = do
        state <- alloca $ (>>) . alGetSourcei (getSource audio) al_SOURCE_STATE <*> peek
        when (state == al_PLAYING) $ alSourceStop (getSource audio)
        
        with (getSource audio) $ alDeleteSources 1
        with (getBuffer audio) $ alDeleteBuffers 1
        alcMakeContextCurrent nullPtr
        alcDestroyContext (getContext audio)
        alcCloseDevice (getDevice audio)

loadWaveHeader header
    | BS.length header /= 44 = putStrLn "Not enough header data." >> return (False,(0,0,0))
    | (toString . BS.take 4) header /= "RIFF" ||
        (toString . BS.take 4 . BS.drop 8) header /= "WAVE" ||
        (toString . BS.take 4 . BS.drop 36) header /= "data" = putStrLn "Not a valid WAVE file." >> return (False,(0,0,0))
    | otherwise = do
        let samplerate = readIntegral 4 24 header
            size = readIntegral 4 40 header
            format = case (readIntegral 2 22 header,readIntegral 2 34 header) of
                (1,8) -> al_FORMAT_MONO8
                (1,16) -> al_FORMAT_MONO16
                (2,8) -> al_FORMAT_STEREO8
                (2,16) -> al_FORMAT_STEREO16
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
