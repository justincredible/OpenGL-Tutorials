module SpecularMapShader (SpecularMapShader,initialize,parameters) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GL

import Flow.Render
import Flow.Shutdown
import ShaderCompilinker

data SpecularMapShader = SpecularMapShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getCameraPosition :: GLint,
    getDirectionLocation :: GLint,
    getDiffuseLocation :: GLint,
    getSpecularLocation :: GLint,
    getPowerLocation :: GLint,
    getTextureLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/specularmap.vert", "glsl/specularmap.frag"]
    
    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        camerapos <- withArray0 0 (map castCharToCChar "camerapos") $ glGetUniformLocation program
        direction <- withArray0 0 (map castCharToCChar "direction") $ glGetUniformLocation program
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ glGetUniformLocation program
        specular <- withArray0 0 (map castCharToCChar "specular") $ glGetUniformLocation program
        power <- withArray0 0 (map castCharToCChar "power") $ glGetUniformLocation program
        texlocn <- withArray0 0 (map castCharToCChar "texas") $ glGetUniformLocation program
        
        let success = all (/= -1) [world,view,projection,camerapos,direction,diffuse,specular,power,texlocn]
        return (success, Just $ SpecularMapShader
            program shaders world view projection camerapos direction diffuse specular power texlocn)

instance Shutdown SpecularMapShader where
    shutdown (SpecularMapShader program shaders _ _ _ _ _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (SpecularMapShader program _ world view projection camerapos direction diffuse specular power texlocn)
    worldMatrix viewMatrix projectionMatrix camposition lightdir lightdif lightspec lightpow texunit = do
        glUseProgram program

        withArray worldMatrix $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMatrix $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMatrix $ glUniformMatrix4fv projection 1 GL_FALSE
        
        withArray camposition $ glUniform3fv camerapos 1
        
        withArray lightdir $ glUniform3fv direction 1
        
        withArray lightdif $ glUniform4fv diffuse 1
        
        withArray lightspec $ glUniform4fv specular 1
        
        glUniform1f power lightpow

        glUniform1i texlocn texunit
