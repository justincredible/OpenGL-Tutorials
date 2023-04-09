module BumpMapShader (BumpMapShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data BumpMapShader = BumpMapShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getColorTexLocation :: GLint,
    getNormalTexLocation :: GLint,
    getDirectionLocation :: GLint,
    getDiffuseLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/bumpmap.vert", "glsl/bumpmap.frag"]
    
    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        clrtex <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        nrmtex <- withArray0 0 (map castCharToCChar "nrmtex") $ glGetUniformLocation program
        direction <- withArray0 0 (map castCharToCChar "direction") $ glGetUniformLocation program
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,clrtex,nrmtex,direction,diffuse]
        return (success, Just $ BumpMapShader
            program shaders world view projection clrtex nrmtex direction diffuse)

instance Shutdown BumpMapShader where
    shutdown (BumpMapShader program shaders _ _ _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (BumpMapShader program _ world view projection clrtex nrmtex direction diffuse)
    worldMatrix viewMatrix projectionMatrix colortex normaltex lightdir lightdif = do
        glUseProgram program

        withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE

        glUniform1i clrtex colortex

        glUniform1i nrmtex normaltex
        
        withArray lightdir $ glUniform3fv direction 1
        
        withArray lightdif $ glUniform4fv diffuse 1
