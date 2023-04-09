module TessellationShader (TessellationShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data TessellationShader = TessellationShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getTessellationAmount :: GLint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getTextureLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink
        ["glsl/tessellation.vert","glsl/tessellation.frag","glsl/tessellation.tcs","glsl/tessellation.tes"]
    
    if not success
    then return (False, Nothing)
    else do
        tessamt <- withArray0 0 (map castCharToCChar "tessamt") $ glGetUniformLocation program
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program

        let success = all (/= -1) [world,view,projection,texture]
        return (success, Just $ TessellationShader program shaders tessamt world view projection texture)

instance Shutdown TessellationShader where
    shutdown (TessellationShader program shaders _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (TessellationShader program _ tessamt world view projection texture)
    worldMx viewMx projectionMx texunit tessellation = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE
        
        glUniform1i texture texunit
        
        glUniform1i tessamt tessellation
