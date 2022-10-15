module ShaderCompilinker (
    compileAndLink,
    outputShaderErrorMessage,
    outputLinkerErrorMessage)
    where

import Prelude hiding (readFile)
import Data.ByteString hiding (map,writeFile,putStrLn)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

compileAndLink vsFile fsFile = do
    vsBuffer <- (fmap (map fromIntegral . unpack) . readFile $ vsFile) :: IO [GLchar]
    fsBuffer <- (fmap (map fromIntegral . unpack) . readFile $ fsFile) :: IO [GLchar]
    
    vShader <- glCreateShader GL_VERTEX_SHADER
    fShader <- glCreateShader GL_FRAGMENT_SHADER
    
    withArray0 0 vsBuffer $ \ptr ->
        with ptr $ \ptrptr ->
            glShaderSource vShader 1 ptrptr nullPtr
        
    withArray0 0 fsBuffer $ \ptr ->
        with ptr $ \ptrptr ->
        glShaderSource fShader 1 ptrptr nullPtr
        
    glCompileShader vShader
    glCompileShader fShader
    
    status <- alloca $ \ptr -> do
        glGetShaderiv vShader GL_COMPILE_STATUS ptr
        peek ptr
    if (status /= 1)
    then do
        putStrLn "Error compiling vertex shader."
        outputShaderErrorMessage vShader vsFile
        return (False, 0, 0, 0)
    else do
        status <- alloca $ \ptr -> do
            glGetShaderiv fShader GL_COMPILE_STATUS ptr
            peek ptr
        if (status /= 1)
        then do
            putStrLn "Error compiling fragment shader."
            outputShaderErrorMessage fShader fsFile
            return (False, 0, 0, 0)
        else do
            program <- glCreateProgram
            
            glAttachShader program vShader
            glAttachShader program fShader
                
            glLinkProgram program
            
            status <- alloca $ \ptr -> do
                glGetProgramiv program GL_LINK_STATUS ptr
                peek ptr

            if (status /= 1)
            then do
                putStrLn "Error linking shader program."
                outputLinkerErrorMessage program
                return (False, 0, 0, 0)
            else return (True, program, vShader, fShader)

outputShaderErrorMessage shaderID shaderFile = do
    logSize <- alloca $ \ptr -> do
        glGetShaderiv shaderID GL_INFO_LOG_LENGTH ptr
        peek ptr
    
    let logSizeI = fromIntegral logSize
    infoLog <- allocaArray logSizeI $ \ptr -> do
        glGetShaderInfoLog shaderID logSize nullPtr ptr
        peekArray logSizeI ptr
        
    writeFile "shader-error.txt" (shaderFile ++ ":\n" ++ map castCCharToChar infoLog)
    
outputLinkerErrorMessage programID = do
    logSize <- alloca $ \ptr -> do
        glGetProgramiv programID GL_INFO_LOG_LENGTH ptr
        peek ptr
    
    let logSizeI = fromIntegral logSize
    infoLog <- allocaArray logSizeI $ \ptr -> do
        glGetProgramInfoLog programID logSize nullPtr ptr
        peekArray logSizeI ptr

    writeFile "linker-error.txt" $ map castCCharToChar infoLog
