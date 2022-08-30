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
    getCameraLocation :: GLint,
    getDirectionLocation :: GLint,
    getAmbientLocation :: GLint,
    getDiffuseLocation :: GLint,
    getSpecularLocation :: GLint,
    getPowerLocation :: GLint }
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
        camera <- withArray0 0 (map castCharToCChar "camera") $ \ptr ->
            glGetUniformLocation program ptr
        tex0 <- withArray0 0 (map castCharToCChar "tex0") $ \ptr ->
            glGetUniformLocation program ptr
        direction <- withArray0 0 (map castCharToCChar "direction") $ \ptr ->
            glGetUniformLocation program ptr
        ambient <- withArray0 0 (map castCharToCChar "ambient") $ \ptr ->
            glGetUniformLocation program ptr
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ \ptr ->
            glGetUniformLocation program ptr
        specular <- withArray0 0 (map castCharToCChar "specular") $ \ptr ->
            glGetUniformLocation program ptr
        power <- withArray0 0 (map castCharToCChar "power") $ \ptr ->
            glGetUniformLocation program ptr

        let success = all (/= -1) [world,view,projection,camera,tex0,direction,ambient,diffuse,specular,power]
        return . (,) success . Just $ LightShader
            program vShader fShader
            world view projection
            tex0
            camera
            direction ambient diffuse specular power

instance Shutdown LightShader where
    shutdown (LightShader program vShader fShader _ _ _ _ _ _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (LightShader program _ _ world view projection texture camera direction ambient diffuse specular power)
    worldMx viewMx projectionMx texID cameraPos lightDir ambientClr diffuseClr specularClr specularPwr = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE

        withArray cameraPos $ glUniform3fv camera 1

        glUniform1i texture texID

        withArray lightDir $ glUniform3fv direction 1

        withArray ambientClr $ glUniform4fv ambient 1

        withArray diffuseClr $ glUniform4fv diffuse 1

        withArray specularClr $ glUniform4fv specular 1

        glUniform1f power specularPwr
