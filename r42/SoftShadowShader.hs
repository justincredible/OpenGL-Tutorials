module SoftShadowShader (SoftShadowShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data SoftShadowShader = SoftShadowShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getLightPositionLocation :: GLint,
    getTextureLocation :: GLint,
    getShadowLocation :: GLint,
    getAmbientLocation :: GLint,
    getDiffuseLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/softshadow.vert","glsl/softshadow.frag"]

    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        lightpos <- withArray0 0 (map castCharToCChar "lightposition") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        shadow <- withArray0 0 (map castCharToCChar "shadow") $ glGetUniformLocation program
        ambient <- withArray0 0 (map castCharToCChar "ambient") $ glGetUniformLocation program
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ glGetUniformLocation program

        let success = all (/= -1) [world,view,projection,lightpos,texture,shadow,ambient,diffuse]
        return (success, Just $
            SoftShadowShader program shaders world view projection lightpos texture shadow ambient diffuse)

instance Shutdown SoftShadowShader where
    shutdown (SoftShadowShader program shaders _ _ _ _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (SoftShadowShader program _ world view projection lightpos texture shadow ambient diffuse)
    worldMx viewMx projectionMx position texunit shadunit ambientClr diffuseClr = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE

        withArray position $ glUniform3fv lightpos 1

        glUniform1i texture texunit

        glUniform1i shadow shadunit

        withArray ambientClr $ glUniform4fv ambient 1

        withArray diffuseClr $ glUniform4fv diffuse 1
