module ColorShader (ColorShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data ColorShader = ColorShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/color.vert", "glsl/color.frag"]
    
    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program

        let success = world /= -1 && view /= -1 && projection /= -1
        return (success, Just $ ColorShader program shaders world view projection)

instance Shutdown ColorShader where
    shutdown (ColorShader program shaders _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (ColorShader program _ world view projection) worldMatrix viewMatrix projectionMatrix = do
    glUseProgram program

    withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

    withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

    withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE
