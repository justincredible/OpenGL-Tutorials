module GlassShader (GlassShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data GlassShader = GlassShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getColorTexLocation :: GLint,
    getNormalTexLocation :: GLint,
    getRefracTexLocation :: GLint,
    getRefraScaleLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/glass.vert" "glsl/glass.frag"
    
    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        colortex <- withArray0 0 (map castCharToCChar "colortex") $ glGetUniformLocation program
        normaltex <- withArray0 0 (map castCharToCChar "normaltex") $ glGetUniformLocation program
        refractex <- withArray0 0 (map castCharToCChar "refractex") $ glGetUniformLocation program
        refrascale <- withArray0 0 (map castCharToCChar "refrascale") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,colortex,normaltex,refractex,refrascale]
        return (success, Just $ GlassShader
            program vShader fShader world view projection colortex normaltex refractex refrascale)

instance Shutdown GlassShader where
    shutdown (GlassShader program vShader fShader _ _ _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (GlassShader program _ _ world view projection colortex normaltex refractex refrascale)
    worldMatrix viewMatrix projectionMatrix clrunit nrmlunit refrunit scale = do
        glUseProgram program

        withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE

        glUniform1i colortex clrunit
        
        glUniform1i normaltex nrmlunit
        
        glUniform1i refractex refrunit
        
        glUniform1f refrascale scale
