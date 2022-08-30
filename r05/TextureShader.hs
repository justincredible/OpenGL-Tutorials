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
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getTextureLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/texture.vert" "glsl/texture.frag"
    
    if not success
    then return (False, Nothing)
    else do
        withArray (map castCharToCChar "position") $ \ptr ->
            glBindAttribLocation program 0 ptr
        withArray (map castCharToCChar "texcoord") $ \ptr ->
            glBindAttribLocation program 1 ptr
        
        world <- withArray (map castCharToCChar "world") $ \ptr ->
            glGetUniformLocation program ptr
        view <- withArray (map castCharToCChar "view") $ \ptr ->
            glGetUniformLocation program ptr
        projection <- withArray (map castCharToCChar "projection") $ \ptr ->
            glGetUniformLocation program ptr
        tex0 <- withArray (map castCharToCChar "tex0") $ \ptr ->
            glGetUniformLocation program ptr

        let success = world /= -1 && view /= -1 && projection /= -1 && tex0 /= -1
        return (success, Just $ TextureShader program vShader fShader world view projection tex0)

instance Shutdown TextureShader where
    shutdown (TextureShader program vShader fShader _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (TextureShader program _ _ world view projection texture) worldMatrix viewMatrix projectionMatrix texUnit = do    
    glUseProgram program

    withArray worldMatrix $ \ptr ->
        glUniformMatrix4fv world 1 GL_FALSE ptr

    withArray viewMatrix $ \ptr ->
        glUniformMatrix4fv view 1 GL_FALSE ptr

    withArray projectionMatrix $ \ptr ->
        glUniformMatrix4fv projection 1 GL_FALSE ptr

    glUniform1i texture texUnit
