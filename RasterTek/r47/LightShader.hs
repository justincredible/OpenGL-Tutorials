module LightShader (LightShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data LightShader = LightShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getTextureLocation :: GLint,
    getCameraPosLocation :: GLint,
    getDirectionLocation :: GLint,
    getAmbientLocation :: GLint,
    getDiffuseLocation :: GLint,
    getSpecularLocation :: GLint,
    getPowerLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/light.vert", "glsl/light.frag"]

    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        camerapos <- withArray0 0 (map castCharToCChar "camerapos") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        direction <- withArray0 0 (map castCharToCChar "direction") $ glGetUniformLocation program
        ambient <- withArray0 0 (map castCharToCChar "ambient") $ glGetUniformLocation program
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ glGetUniformLocation program
        specular <- withArray0 0 (map castCharToCChar "specular") $ glGetUniformLocation program
        power <- withArray0 0 (map castCharToCChar "power") $ glGetUniformLocation program

        let success = all (/= -1) [world,view,projection,camerapos,texture,direction,ambient,diffuse,specular,power]
        return (success, Just $
            LightShader program shaders world view projection texture camerapos direction ambient diffuse specular power)

instance Shutdown LightShader where
    shutdown (LightShader program shaders _ _ _ _ _ _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (LightShader program _ world view projection texture camerapos direction ambient diffuse specular power)
    worldMx viewMx projectionMx texunit position lightDir ambientClr diffuseClr specularClr specularPwr = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE

        withArray position $ glUniform3fv camerapos 1

        glUniform1i texture texunit

        withArray lightDir $ glUniform3fv direction 1

        withArray ambientClr $ glUniform4fv ambient 1

        withArray diffuseClr $ glUniform4fv diffuse 1

        withArray specularClr $ glUniform4fv specular 1

        glUniform1f power specularPwr
