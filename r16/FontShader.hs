module FontShader (FontShader,initialize,parameters,pixelColour) where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data FontShader = FontShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getTextureLocation :: GLint,
    getPixelColour :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/font.vert" "glsl/font.frag"
    
    if not success
    then return (False, Nothing)
    else do
        withArray (map castCharToCChar "position") $ glBindAttribLocation program 0
        withArray (map castCharToCChar "texcoord") $ glBindAttribLocation program 1
        
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        texlocn <- withArray0 0 (map castCharToCChar "tex0") $ glGetUniformLocation program
        pxlclr <- withArray0 0 (map castCharToCChar "pxlclr") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,texlocn,pxlclr]
        return (success, Just $ FontShader
            program vShader fShader world view projection texlocn pxlclr)

instance Shutdown FontShader where
    shutdown (FontShader program vShader fShader _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (FontShader program _ _ world view projection texlocn pxlclr)
    worldMatrix viewMatrix projectionMatrix texunit pixclr = do
        glUseProgram program

        withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE

        glUniform1i texlocn texunit
        
        withArray pixclr $ glUniform4fv pxlclr 1

pixelColour = getPixelColour
