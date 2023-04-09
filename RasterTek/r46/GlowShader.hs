module GlowShader (GlowShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data GlowShader = GlowShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getTextureLocation :: GLint,
    getGlowMapLocation :: GLint,
    getStrengthLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/glow.vert","glsl/glow.frag"]
    
    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        texlocn <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        glowtex <- withArray0 0 (map castCharToCChar "glow") $ glGetUniformLocation program
        strength <- withArray0 0 (map castCharToCChar "strength") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,texlocn,glowtex,strength]
        return (success, Just $ GlowShader
            program shaders world view projection texlocn glowtex strength)

instance Shutdown GlowShader where
    shutdown (GlowShader program shaders _ _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (GlowShader program _ world view projection texlocn glowtex strength)
    worldMatrix viewMatrix projectionMatrix texunit glowunit glow = do
        glUseProgram program

        withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE

        glUniform1i texlocn texunit
        
        glUniform1i glowtex glowunit
        
        glUniform1f strength glow
