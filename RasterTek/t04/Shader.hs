module Shader (Shader, initialize, render) where

import Graphics.Rendering.OpenGL as GL hiding (Shader)

import ShaderManager

data Shader = Shader Program

initialize = do
    program <- loadShaders [ ShaderInfo VertexShader (FileSource "glsl/color.vert"), ShaderInfo FragmentShader (FileSource "glsl/color.frag") ]
    return $ Shader program

render (Shader program) worldMatrix viewMatrix projectionMatrix = do
    currentProgram $= Just program
    
    world <- get (uniformLocation program "world")
    uniform world $= worldMatrix
    
    view <- get (uniformLocation program "view")
    uniform view $= viewMatrix
    
    projection <- get (uniformLocation program "projection")
    uniform projection $= projectionMatrix
