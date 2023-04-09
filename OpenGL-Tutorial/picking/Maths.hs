module Maths where

{- Maths:
    Linear Algebra functions for vectors and matrices
-}

import Graphics.GL

import Data.List

add :: Num a => [a] -> [a] -> [a]
add = zipWith (+)

minus a b = add a $ map negate b

multiply3 [x,y,z] [[m11,m21,m31],[m12,m22,m32],[m13,m23,m33]] =
    [ x*m11 + y*m12 + z*m13, x*m21 + y*m22 + z*m23, x*m31 + y*m32 + z*m33 ]

matscale4 s = map (map (*s))
matmult4 a b = ($ []) . (flip (flip foldl' id)) b $ \dl bs -> (dl.).(:) $ [
    foldr (+) 0 . zipWith (*) (map head a) $ bs,
    foldr (+) 0 . zipWith (*) (map (head.tail) $ a) $ bs,
    foldr (+) 0 . zipWith (*) (map (head.tail.tail) $ a) $ bs,
    foldr (+) 0 . zipWith (*) (map (head.tail.tail.tail) $ a) $ bs ]

transpose4 [[m11,m21,m31,m41],[m12,m22,m32,m42],[m13,m23,m33,m43],[m14,m24,m34,m44]] =
    [[m11,m12,m13,m14],[m21,m22,m23,m24],[m31,m32,m33,m34],[m41,m42,m43,m44]]

-- Right-Hand cross
cross [x,y,z] [x',y',z'] = [ y*z' - z*y', z*x' - x*z', x*y' - y*x' ]

dot :: Num a => [a] -> [a] -> a
dot = (foldr (+) 0 .) . zipWith (*)

normalise :: Floating a => [a] -> [a]
normalise = map . flip (/) . sqrt . foldr ((+).(**2)) 0 <*> id

identity4 :: [[GLfloat]]
identity4 = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]

scale4 x y z = [[x,0,0,0],[0,y,0,0],[0,0,z,0],[0,0,0,1]]

translate4 x y z = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[x,y,z,1]]

rotateX4 angle = let sa = sin angle; ca = cos angle in
    [[1,0,0,0],[0,ca,sa,0],[0,-sa,ca,0],[0,0,0,1]]
    
rotateY4 angle = let sa = sin angle; ca = cos angle in
    [[ca,0,-sa,0],[0,1,0,0],[sa,0,ca,0],[0,0,0,1]]
    
rotateZ4 angle = let sa = sin angle; ca = cos angle in
    [[ca,sa,0,0],[-sa,ca,0,0],[0,0,1,0],[0,0,0,1]]
    
view4 position lookat up = let
    zAxis@[zax,zay,zaz] = (normalise . minus lookat) position
    xAxis@[xax,xay,xaz] = (normalise . cross zAxis) up
    yAxis@[yax,yay,yaz] = cross xAxis zAxis
    in
    [ [xax, yax, zax, 0]
    , [xay, yay, zay, 0]
    , [xaz, yaz, zaz, 0]
    , [negate . dot xAxis $ position, negate . dot yAxis $ position, negate . dot zAxis $ position, 1 ] ]
    
perspective4 fieldOfView aspect near far = let
    rtfov = recip . tan . (*0.5) $ fieldOfView
    denom = far - near
    in
    [ [rtfov*recip aspect, 0, 0, 0]
    , [0, rtfov, 0, 0]
    , [0, 0, (far+near)/denom, 1]
    , [0, 0, negate $ far*near/denom, 0 ] ]

orthographic4 left right bottom top near zfar =
    [ [2/(right-left), 0, 0, 0]
    , [0, 2/(top-bottom), 0, 0]
    , [0, 0, 2/(zfar-near), 0]
    , [ -(right+left)/(right-left), -(top+bottom)/(top-bottom), -(zfar+near)/(zfar-near), 1] ]

rotationYawPitchRoll3 yaw pitch roll = [
    [ cos yaw*cos pitch, sin yaw*cos pitch, -sin pitch ]
    , [ cos yaw*sin pitch*sin roll - sin yaw*cos roll, sin yaw*sin pitch*sin roll + cos yaw*cos roll, cos pitch*sin roll ]
    , [ cos yaw*sin pitch*cos roll + sin yaw*sin roll, sin yaw*sin pitch*cos roll - cos yaw*sin roll, cos pitch*cos roll ] ]

rotationYawPitchRoll4 yaw pitch roll = [
    [ cos yaw*cos pitch, sin yaw*cos pitch, -sin pitch, 0 ]
    , [ cos yaw*sin pitch*sin roll - sin yaw*cos roll, sin yaw*sin pitch*sin roll + cos yaw*cos roll, cos pitch*sin roll, 0 ]
    , [ cos yaw*sin pitch*cos roll + sin yaw*sin roll, sin yaw*sin pitch*cos roll - cos yaw*sin roll, cos pitch*cos roll, 0 ]
    , [ 0, 0, 0, 1] ]
