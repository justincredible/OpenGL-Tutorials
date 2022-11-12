module TextureShader (TextureShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data TextureShader = TextureShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getTextureLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/texture.vert", "glsl/texture.frag"]
    
    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program

        let success = world /= -1 && view /= -1 && projection /= -1 && texture /= -1
        return (success, Just $ TextureShader program shaders world view projection texture)

instance Shutdown TextureShader where
    shutdown (TextureShader program shaders _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (TextureShader program _ world view projection texture) worldMatrix viewMatrix projectionMatrix texUnit = do    
    glUseProgram program

    withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

    withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

    withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE

    glUniform1i texture texUnit
