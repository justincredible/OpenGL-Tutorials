module Camera (Camera,initialize,render) where

import Graphics.Rendering.OpenGL as GL hiding (normalize)

import OpenGL

data Camera = Camera (Vector3 GLfloat) (Vector3 GLfloat)

initialize :: IO Camera
initialize = return $ Camera (Vector3 0 0 (-10)) (Vector3 0 0 0)

render :: Camera -> IO (GLmatrix GLfloat)
render (Camera position rotation) = do
    let rMatrix = matrixRotationPitchYawRoll rotation
        lookat = add position $ multiply3 (Vector3 0 0 1) rMatrix
        up = multiply3 (Vector3 0 1 0) rMatrix
    
    buildViewMatrix position lookat up

matrixRotationPitchYawRoll (Vector3 pitch yaw roll) = let
    cpitch = cos pitch
    cyaw = cos yaw
    croll = cos roll
    spitch = sin pitch
    syaw = sin yaw
    sroll = sin roll
    in
    [ Vector3 (croll*cyaw + spitch*sroll*syaw) (sroll*cpitch) (croll*(-syaw) + spitch*sroll*cyaw)
    , Vector3 ((-sroll)*cyaw + spitch*croll*syaw) (croll*cpitch) (sroll*syaw + spitch*croll*cyaw)
    , Vector3 (cpitch*syaw) (-spitch) (cpitch*cyaw) ]

buildViewMatrix position@(Vector3 x y z) lookat@(Vector3 lx ly lz) up@(Vector3 ux uy uz) = do
    let zAxis@(Vector3 zax zay zaz) = (normalize . minus lookat) position
        xAxis@(Vector3 xax xay xaz) = (normalize . cross up) zAxis
        yAxis@(Vector3 yax yay yaz) = cross zAxis xAxis
    newMatrix ColumnMajor [xax,yax,zax,0,xay,yay,zay,0,xaz,yaz,zaz,0,negate . dot xAxis $ position,negate . dot yAxis $ position,negate . dot zAxis $ position,1]
