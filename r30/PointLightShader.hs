module PointLightShader (PointLightShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data PointLightShader = PointLightShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getLightPosLocation :: GLint,
    getTextureLocation :: GLint,
    getDiffuseClrsLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/pointlight.vert" "glsl/pointlight.frag"

    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        lightpos <- withArray0 0 (map castCharToCChar "lightpositions") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        diffuse <- withArray0 0 (map castCharToCChar "diffuseClrs") $ glGetUniformLocation program

        let success = all (/= -1) [world,view,projection,lightpos,texture,diffuse]
        return (success, Just $
            PointLightShader program vShader fShader world view projection lightpos texture diffuse)

instance Shutdown PointLightShader where
    shutdown (PointLightShader program vShader fShader _ _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (PointLightShader program _ _ world view projection lightpos texture diffuse)
    worldMx viewMx projectionMx positions texunit diffuseClrs = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE
        
        withArray positions $ glUniform3fv lightpos nUM_LIGHTS

        glUniform1i texture texunit

        withArray diffuseClrs $ glUniform4fv diffuse nUM_LIGHTS

nUM_LIGHTS = 4
