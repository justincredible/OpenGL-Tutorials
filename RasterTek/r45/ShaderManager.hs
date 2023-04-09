module ShaderManager (ShaderManager,ShaderManager.initialize) where

import BumpMapShader
import Flow.Parameters
import Flow.Shutdown
import Flow.Update
import LightShader
import OpenGL
import TextureShader

data ShaderManager = ShaderManager {
    getTextureShader :: TextureShader,
    getLightShader :: LightShader,
    getBumpMapShader :: BumpMapShader }
    deriving (Eq, Show)

initialize = do
    (tscs,Just tshader) <- TextureShader.initialize
    (lscs,Just lshader) <- LightShader.initialize
    (bmscs,Just bmshader) <- BumpMapShader.initialize
    return (and [tscs,lscs,bmscs], ShaderManager tshader lshader bmshader)

instance Update ShaderManager where
    update manager (ShaderTexture world view projection texture) = do
        TextureShader.parameters (getTextureShader manager) world view projection texture
        return (True,manager)

    update manager (ShaderLight world view projection camerapos texture direction ambient diffuse specular power) = do
        LightShader.parameters (getLightShader manager)
            world view projection camerapos texture
            direction ambient diffuse specular power
        return (True,manager)

    update manager (ShaderBumpMap world view projection color normal direction diffuse) = do
        BumpMapShader.parameters (getBumpMapShader manager)
            world view projection
            color normal
            direction diffuse
        return (True,manager)

    update manager _ = do
        putStrLn "Incorrect shader manager parameters."
        return (False, manager)

instance Shutdown ShaderManager where
    shutdown manager = do
        shutdown . getTextureShader $ manager
        shutdown . getLightShader $ manager
        shutdown . getBumpMapShader $ manager
