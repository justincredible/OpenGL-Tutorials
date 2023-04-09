module ShadowShader (ShadowShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

nUM_LIGHTS = 2

data ShadowShader = ShadowShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getLightViewsLocation :: GLint,
    getLightProjectsLocation :: GLint,
    getLightPosLocation :: GLint,
    getTextureLocation :: GLint,
    getDepthMapsLocation :: GLint,
    getAmbientLocation :: GLint,
    getDiffusesLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/shadow.vert", "glsl/shadow.frag"]

    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        lightviews <- withArray0 0 (map castCharToCChar "lightviews") $ glGetUniformLocation program
        lightprojects <- withArray0 0 (map castCharToCChar "lightprojects") $ glGetUniformLocation program
        lightpos <- withArray0 0 (map castCharToCChar "lightpositions") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        depths <- withArray0 0 (map castCharToCChar "depths") $ glGetUniformLocation program
        ambient <- withArray0 0 (map castCharToCChar "ambient") $ glGetUniformLocation program
        diffuses <- withArray0 0 (map castCharToCChar "diffuses") $ glGetUniformLocation program

        let success = all (/= -1) [world,view,projection,lightviews,lightprojects,lightpos,texture,depths,ambient,diffuses]
        return (success, Just $
            ShadowShader program shaders world view projection lightviews lightprojects lightpos texture depths ambient diffuses)

instance Shutdown ShadowShader where
    shutdown (ShadowShader program shaders _ _ _ _ _ _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (ShadowShader program _ world view projection lightviews lightprojects lightpos texture depths ambient diffuses)
    worldMx viewMx projectionMx lightviewMcs lightprojectMcs positions texunit depthunits ambientClr diffuseClrs = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE

        withArray lightviewMcs $ glUniformMatrix4fv lightviews nUM_LIGHTS GL_FALSE

        withArray lightprojectMcs $ glUniformMatrix4fv lightprojects nUM_LIGHTS GL_FALSE
        
        withArray positions $ glUniform3fv lightpos nUM_LIGHTS

        glUniform1i texture texunit
        
        withArray depthunits $ glUniform1iv depths nUM_LIGHTS
        
        withArray ambientClr $ glUniform4fv ambient 1

        withArray diffuseClrs $ glUniform4fv diffuses nUM_LIGHTS
