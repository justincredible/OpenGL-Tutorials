module ProjectionShader (ProjectionShader,initialize,parameters) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GL

import Flow.Shutdown
import ShaderCompilinker

data ProjectionShader = ProjectionShader {
    getProgram :: GLuint,
    getShaders :: [GLuint],
    getWorldLocation :: GLint,
    getViewLocation :: GLint,
    getProjectionLocation :: GLint,
    getViewPointLocation :: GLint,
    getProjectPointLocation :: GLint,
    getTextureLocation :: GLint,
    getProjectTexLocation :: GLint,
    getDirectionLocation :: GLint,
    getAmbientLocation :: GLint,
    getDiffuseLocation :: GLint }
    deriving (Eq, Show)

initialize = do
    (success, program, shaders) <- compileAndLink ["glsl/projection.vert", "glsl/projection.frag"]

    if not success
    then return (False, Nothing)
    else do
        world <- withArray0 0 (map castCharToCChar "world") $ glGetUniformLocation program
        view <- withArray0 0 (map castCharToCChar "view") $ glGetUniformLocation program
        projection <- withArray0 0 (map castCharToCChar "projection") $ glGetUniformLocation program
        viewpnt <- withArray0 0 (map castCharToCChar "viewpnt") $ glGetUniformLocation program
        projectpnt <- withArray0 0 (map castCharToCChar "projectpnt") $ glGetUniformLocation program
        texture <- withArray0 0 (map castCharToCChar "ture") $ glGetUniformLocation program
        projectex <- withArray0 0 (map castCharToCChar "project") $ glGetUniformLocation program
        direction <- withArray0 0 (map castCharToCChar "direction") $ glGetUniformLocation program
        ambient <- withArray0 0 (map castCharToCChar "ambient") $ glGetUniformLocation program
        diffuse <- withArray0 0 (map castCharToCChar "diffuse") $ glGetUniformLocation program

        let success = all (/= -1) [world,view,projection,viewpnt,projectpnt,texture,projectex,direction,ambient,diffuse]
        return (success, Just $
            ProjectionShader program shaders world view projection viewpnt projectpnt texture projectex direction ambient diffuse)

instance Shutdown ProjectionShader where
    shutdown (ProjectionShader program shaders _ _ _ _ _ _ _ _ _ _) = do
        sequence_ $ map (glDetachShader program) shaders
        
        sequence_ $ map glDeleteShader shaders
        
        glDeleteProgram program

parameters (ProjectionShader program _ world view projection viewpnt projectpnt texture projectex direction ambient diffuse)
    worldMx viewMx projectionMx viewPntMx projectPntMx texunit projectexunit lightDir ambientClr diffuseClr = do
        glUseProgram program

        withArray worldMx $ glUniformMatrix4fv world 1 GL_FALSE

        withArray viewMx $ glUniformMatrix4fv view 1 GL_FALSE

        withArray projectionMx $ glUniformMatrix4fv projection 1 GL_FALSE

        withArray viewPntMx $ glUniformMatrix4fv viewpnt 1 GL_FALSE

        withArray projectPntMx $ glUniformMatrix4fv projectpnt 1 GL_FALSE

        glUniform1i texture texunit

        glUniform1i projectex projectexunit

        withArray lightDir $ glUniform3fv direction 1

        withArray ambientClr $ glUniform4fv ambient 1

        withArray diffuseClr $ glUniform4fv diffuse 1
