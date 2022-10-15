module LightShader (LightShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data LightShader = LightShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getTextureLocation :: GLint,
    getDirectionLocation :: GLint,
    getDiffuseLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/light.vert" "glsl/light.frag"

    if not success
    then return (False, Nothing)
    else do
        withArray0 0 (map castCharToCChar "pos") $ \ptr ->
            glBindAttribLocation program 0 ptr
        withArray0 0 (map castCharToCChar "tex") $ \ptr ->
            glBindAttribLocation program 1 ptr
        withArray0 0 (map castCharToCChar "nml") $ \ptr ->
            glBindAttribLocation program 2 ptr
        
        world <- withArray0 0 (map castCharToCChar "world") $ \ptr ->
            glGetUniformLocation program ptr
        view <- withArray0 0 (map castCharToCChar "view") $ \ptr ->
            glGetUniformLocation program ptr
        projection <- withArray0 0 (map castCharToCChar "projection") $ \ptr ->
            glGetUniformLocation program ptr
        tex0 <- withArray0 0 (map castCharToCChar "tex0") $ \ptr ->
            glGetUniformLocation program ptr
        direction <- withArray0 0 (map castCharToCChar "direction") $ \ptr ->
            glGetUniformLocation program ptr
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ \ptr ->
            glGetUniformLocation program ptr

        let success = all (/= -1) [world,view,projection,tex0,direction,diffuse]
        return . (,) success . Just $ LightShader
            program vShader fShader
            world view projection
            tex0
            direction diffuse

instance Shutdown LightShader where
    shutdown (LightShader program vShader fShader _ _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (LightShader program _ _ world view projection texture direction diffuse)
    worldMx viewMx projectionMx texunit lightDir diffuseClr = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE

        glUniform1i texture texunit

        withArray lightDir $ glUniform3fv direction 1

        withArray diffuseClr $ glUniform4fv diffuse 1
