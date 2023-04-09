module ShadowShader (ShadowShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data ShadowShader = ShadowShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getLightViewLocation :: GLint,
    getLightProjectLocation :: GLint,
    getTextureLocation :: GLint,
    getDepthTexLocation :: GLint,
    getAmbientLocation :: GLint,
    getDiffuseLocation :: GLint,
    getDirectionLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/shadow.vert", "glsl/shadow.frag"]

    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        lightview <- withArray0 0 (map castCharToCChar "lightview") $ glGetUniformLocation program
        lightproject <- withArray0 0 (map castCharToCChar "lightproject") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        depthtex <- withArray0 0 (map castCharToCChar "depth") $ glGetUniformLocation program
        ambient <- withArray0 0 (map castCharToCChar "ambient") $ glGetUniformLocation program
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ glGetUniformLocation program
        direction <- withArray0 0 (map castCharToCChar "direction") $ glGetUniformLocation program

        let success = all (/= -1) [world,view,projection,lightview,lightproject,texture,depthtex,ambient,diffuse,direction]
        return (success, Just $
            ShadowShader program shaders world view projection lightview lightproject texture depthtex ambient diffuse direction)

instance Shutdown ShadowShader where
    shutdown (ShadowShader program shaders _ _ _ _ _ _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (ShadowShader program _ world view projection lightview lightproject texture depthtex ambient diffuse direction)
    worldMx viewMx projectionMx lightviewMx lightprojectMx texunit depthunit ambientClr diffuseClr lightdir = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE

        withArray lightviewMx $ glUniformMatrix4fv lightview 1 GL_FALSE

        withArray lightprojectMx $ glUniformMatrix4fv lightproject 1 GL_FALSE

        glUniform1i texture texunit

        glUniform1i depthtex depthunit
        
        withArray ambientClr $ glUniform4fv ambient 1
        
        withArray diffuseClr $ glUniform4fv diffuse 1
        
        withArray lightdir $ glUniform3fv direction 1
