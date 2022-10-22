module FadeShader (FadeShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data FadeShader = FadeShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getTextureLocation :: GLint,
    getFadeAmountLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/fade.vert" "glsl/fade.frag"
    
    if not success
    then return (False, Nothing)
    else do
        withArray (map castCharToCChar "position") $ glBindAttribLocation program 0
        withArray (map castCharToCChar "texcoord") $ glBindAttribLocation program 1
        
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        texlocn <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        fadeamt <- withArray0 0 (map castCharToCChar "fadeamt") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,texlocn,fadeamt]
        return (success, Just $ FadeShader
            program vShader fShader world view projection texlocn fadeamt)

instance Shutdown FadeShader where
    shutdown (FadeShader program vShader fShader _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (FadeShader program _ _ world view projection texlocn fadeamt)
    worldMatrix viewMatrix projectionMatrix texunit fade = do
        glUseProgram program

        withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE

        glUniform1i texlocn texunit
        
        glUniform1f fadeamt fade
