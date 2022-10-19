module FogShader (FogShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data FogShader = FogShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getRangeLocation :: GLint,
    getTextureLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/fog.vert" "glsl/fog.frag"
    
    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        range <- withArray0 0 (map castCharToCChar "range") $ glGetUniformLocation program
        texlocn <- withArray0 0 (map castCharToCChar "image") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,range,texlocn]
        return (success, Just $ FogShader
            program vShader fShader world view projection range texlocn)

instance Shutdown FogShader where
    shutdown (FogShader program vShader fShader _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (FogShader program _ _ world view projection range texlocn)
    worldMatrix viewMatrix projectionMatrix fogrng texunit = do
        glUseProgram program

        withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE
        
        withArray fogrng $ glUniform2fv range 1

        glUniform1i texlocn texunit
