module FireShader (FireShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data FireShader = FireShader {
    getProgram :: GLuint,
    getVertexShader :: GLuint,
    getFragmentShader :: GLuint,
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getFrameTimeLocation :: GLint,
    getScrollsLocation :: GLint,
    getScalesLocation :: GLint,
    getFireLocation :: GLint,
    getNoiseLocation :: GLint,
    getAlphaLocation :: GLint,
    getDistortionsLocation :: GLint,
    getDistortScaleLocation :: GLint,
    getDistortBiasLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, vShader, fShader) <- compileAndLink "glsl/fire.vert" "glsl/fire.frag"
    
    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        frametime <- withArray0 0 (map castCharToCChar "frametime") $ glGetUniformLocation program
        scrolls <- withArray0 0 (map castCharToCChar "scrolls") $ glGetUniformLocation program
        scales <- withArray0 0 (map castCharToCChar "scales") $ glGetUniformLocation program
        fire <- withArray0 0 (map castCharToCChar "fire") $ glGetUniformLocation program
        noise <- withArray0 0 (map castCharToCChar "noise") $ glGetUniformLocation program
        alpha <- withArray0 0 (map castCharToCChar "alpha") $ glGetUniformLocation program
        distortions <- withArray0 0 (map castCharToCChar "distortions") $ glGetUniformLocation program
        distortscale <- withArray0 0 (map castCharToCChar "distortscale") $ glGetUniformLocation program
        distortbias <- withArray0 0 (map castCharToCChar "distortbias") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,frametime,scrolls,scales,fire,noise,alpha,distortions,distortscale,distortbias]
        return (success, Just $ FireShader
            program vShader fShader world view projection frametime scrolls scales fire noise alpha distortions distortscale distortbias)

instance Shutdown FireShader where
    shutdown (FireShader program vShader fShader _ _ _ _ _ _ _ _ _ _ _ _) = do
        glDetachShader program vShader
        glDetachShader program fShader
        
        glDeleteShader vShader
        glDeleteShader fShader
        
        glDeleteProgram program

parameters (FireShader program _ _ world view projection frametime scrolls scales fire noise alpha distortions distortscale distortbias)
    worldMx viewMx projectionMx time speeds scrlscales fireunit noiseunit alphaunit distortVc scale bias = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE
        
        glUniform1f frametime time
        
        withArray speeds $ glUniform3fv scrolls 1
        
        withArray scrlscales $ glUniform3fv scales 1

        glUniform1i fire fireunit
        
        glUniform1i noise noiseunit
        
        glUniform1i alpha alphaunit
        
        withArray (concat distortVc) $ glUniform2fv distortions 3
        
        glUniform1f distortscale scale
        
        glUniform1f distortbias bias
