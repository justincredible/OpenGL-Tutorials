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
    getAmbientLocation :: GLint,
    getDiffuseLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/light.vert" "glsl/light.frag"

    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        direction <- withArray0 0 (map castCharToCChar "direction") $ glGetUniformLocation program
        ambient <- withArray0 0 (map castCharToCChar "ambient") $ glGetUniformLocation program
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ glGetUniformLocation program

        let success = all (/= -1) [world,view,projection,texture,direction,ambient,diffuse]
        return (success, Just $
            LightShader program vShader fShader world view projection texture direction ambient diffuse)

instance Shutdown LightShader where
    shutdown (LightShader program vShader fShader _ _ _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (LightShader program _ _ world view projection texture direction ambient diffuse)
    worldMx viewMx projectionMx texunit lightDir ambientClr diffuseClr = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE

        glUniform1i texture texunit

        withArray lightDir $ glUniform3fv direction 1

        withArray ambientClr $ glUniform4fv ambient 1

        withArray diffuseClr $ glUniform4fv diffuse 1
