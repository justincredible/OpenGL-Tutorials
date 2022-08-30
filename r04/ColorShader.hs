module ColorShader (ColorShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data ColorShader = ColorShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/color.vert" "glsl/color.frag"
    
    if not success
    then return (False, Nothing)
    else do
        withArray (map castCharToCChar "position") $ \ptr ->
            glBindAttribLocation program 0 ptr
        withArray (map castCharToCChar "colorv") $ \ptr ->
            glBindAttribLocation program 1 ptr
        
        world <- withArray (map castCharToCChar "world") $ \ptr ->
            glGetUniformLocation program ptr
        view <- withArray (map castCharToCChar "view") $ \ptr ->
            glGetUniformLocation program ptr
        projection <- withArray (map castCharToCChar "projection") $ \ptr ->
            glGetUniformLocation program ptr

        let success = world /= -1 && view /= -1 && projection /= -1
        return (success, Just $ ColorShader program vShader fShader world view projection)

instance Shutdown ColorShader where
    shutdown (ColorShader program vShader fShader _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (ColorShader program _ _ world view projection) worldMatrix viewMatrix projectionMatrix = do
    glUseProgram program

    withArray worldMatrix $ \ptr ->
        glUniformMatrix4fv world 1 GL_FALSE ptr

    withArray viewMatrix $ \ptr ->
        glUniformMatrix4fv view 1 GL_FALSE ptr

    withArray projectionMatrix $ \ptr ->
        glUniformMatrix4fv projection 1 GL_FALSE ptr
