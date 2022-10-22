module ReflectionShader (ReflectionShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data ReflectionShader = ReflectionShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getReflectionLocation :: GLint,
    getTextureLocation :: GLint,
    getReflecTexLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/reflection.vert" "glsl/reflection.frag"
    
    if not success
    then return (False, Nothing)
    else do
        withArray (map castCharToCChar "position") $ glBindAttribLocation program 0
        withArray (map castCharToCChar "texcoord") $ glBindAttribLocation program 1
        
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        reflection <- withArray0 0 (map castCharToCChar "reflection") $ glGetUniformLocation program
        texlocn <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        reflect <- withArray0 0 (map castCharToCChar "reflect") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,reflection,texlocn,reflect]
        return (success, Just $ ReflectionShader
            program vShader fShader world view projection reflection texlocn reflect)

instance Shutdown ReflectionShader where
    shutdown (ReflectionShader program vShader fShader _ _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (ReflectionShader program _ _ world view projection reflection texlocn reflectex)
    worldMx viewMx projectionMx reflectionMx texunit reflect = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE
        
        withArray reflectionMx $ glUniformMatrix4fv reflection 1 GL_FALSE

        glUniform1i texlocn texunit
        
        glUniform1i reflectex reflect
