module ShaderCompilinker (
    compileAndLink,
    outputShaderErrorMessage,
    outputLinkerErrorMessage)
    where

import Control.Arrow
import Control.Monad
import qualified Data.ByteString as BS
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import System.Directory

compileAndLink shaderFiles = do
    buffers <- sequence $ map (fmap (map fromIntegral . BS.unpack) . BS.readFile) shaderFiles
    
    let types = case length shaderFiles of
            4 -> [GL_VERTEX_SHADER,GL_FRAGMENT_SHADER,GL_TESS_CONTROL_SHADER,GL_TESS_EVALUATION_SHADER]
            2 -> [GL_VERTEX_SHADER,GL_FRAGMENT_SHADER]
            _ -> []
    
    exists <- doesPathExist "shader-error.txt"
    when exists $ writeFile "shader-error.txt" ""
    
    (statuses,shaders) <- fmap (map fst &&& map snd) . sequence $
        zipWith compile types (zip shaderFiles buffers)
    
    (status,program) <- link shaders

    if (any (/= 1) statuses || status /= 1)
    then do
        when (status /= 1) $ do
            putStrLn "Error linking shader program."
            outputLinkerErrorMessage program
        sequence_ $ map (glDetachShader program) shaders
        sequence_ $ map glDeleteShader shaders
        return (False,0,[])
    else
        return (True, program, shaders)
    where
    compile shadertype (file,buffer) = do
        shader <- glCreateShader shadertype
        
        withArray0 0 buffer (flip with $ flip (glShaderSource shader 1) nullPtr)
        
        glCompileShader shader
        
        status <- alloca $ (>>) . glGetShaderiv shader GL_COMPILE_STATUS <*> peek
        
        when (status /= 1) $ do
            putStrLn "Error compiling shader."
            outputShaderErrorMessage shader file
            
        return (status, shader)
    link shaders = do
        program <- glCreateProgram
        
        sequence_ $ map (glAttachShader program) shaders
            
        glLinkProgram program
        
        status <- alloca $ (>>) . glGetProgramiv program GL_LINK_STATUS <*> peek
        
        return (status, program)

outputShaderErrorMessage shaderID shaderFile = do
    logSize <- alloca $ (>>) . glGetShaderiv shaderID GL_INFO_LOG_LENGTH <*> peek
    
    let logSizeI = fromIntegral logSize
    infoLog <- allocaArray logSizeI $ (>>) . glGetShaderInfoLog shaderID logSize nullPtr <*> peekArray logSizeI
        
    appendFile "shader-error.txt" (shaderFile ++ ":\n" ++ map castCCharToChar infoLog ++ "\n")
    
outputLinkerErrorMessage programID = do
    logSize <- alloca $ (>>) . glGetProgramiv programID GL_INFO_LOG_LENGTH <*> peek
    
    let logSizeI = fromIntegral logSize
    infoLog <- allocaArray logSizeI $ (>>) . glGetProgramInfoLog programID logSize nullPtr <*> peekArray logSizeI

    writeFile "linker-error.txt" $ map castCCharToChar infoLog
