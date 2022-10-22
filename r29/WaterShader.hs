module WaterShader (WaterShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data WaterShader = WaterShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getReflectionLocation :: GLint,
    getReflecTexLocation :: GLint,
    getRefracTexLocation :: GLint,
    getNormalTexLocation :: GLint,
    getWaterTransLocation :: GLint,
    getRefScaleLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/water.vert" "glsl/water.frag"
    
    if not success
    then return (False, Nothing)
    else do
        withArray (map castCharToCChar "position") $ glBindAttribLocation program 0
        withArray (map castCharToCChar "texcoord") $ glBindAttribLocation program 1
        
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        reflection <- withArray0 0 (map castCharToCChar "reflection") $ glGetUniformLocation program
        reflectex <- withArray0 0 (map castCharToCChar "reflectex") $ glGetUniformLocation program
        refractex <- withArray0 0 (map castCharToCChar "refractex") $ glGetUniformLocation program
        normaltex <- withArray0 0 (map castCharToCChar "normaltex") $ glGetUniformLocation program
        watertrans <- withArray0 0 (map castCharToCChar "watertrans") $ glGetUniformLocation program
        refscale <- withArray0 0 (map castCharToCChar "refscale") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,reflection,reflectex,refractex,normaltex,watertrans,refscale]
        return (success, Just $ WaterShader
            program vShader fShader world view projection reflection reflectex refractex normaltex watertrans refscale)

instance Shutdown WaterShader where
    shutdown (WaterShader program vShader fShader _ _ _ _ _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (WaterShader program _ _ world view projection reflection reflectex refractex normaltex watertrans refscale)
    worldMx viewMx projectionMx reflectionMx reflunit refrunit normunit translate scale = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE
        
        withArray reflectionMx $ glUniformMatrix4fv reflection 1 GL_FALSE

        glUniform1i reflectex reflunit
        
        glUniform1i refractex refrunit
        
        glUniform1i normaltex normunit
        
        glUniform1f watertrans translate
        
        glUniform1f refscale scale
