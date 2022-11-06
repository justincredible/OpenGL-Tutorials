module Maths where

import Data.List

{- Maths:
    Linear Algebra functions for vectors and matrices
-}

add :: Num a => [a] -> [a] -> [a]
add = zipWith (+)

minus a b = add a $ map negate b

multiply3 [x,y,z] [[m11,m21,m31],[m12,m22,m32],[m13,m23,m33]] =
    [ x*m11 + y*m12 + z*m13
    , x*m21 + y*m22 + z*m23
    , x*m31 + y*m32 + z*m33 ]

multiply4 [x,y,z,w] [[m11,m21,m31,m41],[m12,m22,m32,m42],[m13,m23,m33,m43],[m14,m24,m34,m44]] =
    [ x*m11 + y*m12 + z*m13 + w*m14
    , x*m21 + y*m22 + z*m23 + w*m24
    , x*m31 + y*m32 + z*m33 + w*m34
    , x*m41 + y*m42 + z*m43 + w*m44 ]

matmult4 a b = ($ []) . (flip (flip foldl' id)) b $ \dl bs -> (dl.).(:) $ [
    foldr (+) 0 . zipWith (*) (map head a) $ bs,
    foldr (+) 0 . zipWith (*) (map (head.tail) $ a) $ bs,
    foldr (+) 0 . zipWith (*) (map (head.tail.tail) $ a) $ bs,
    foldr (+) 0 . zipWith (*) (map (head.tail.tail.tail) $ a) $ bs ]

transpose4 [m11,m21,m31,m41,m12,m22,m32,m42,m13,m23,m33,m43,m14,m24,m34,m44] =
    [m11,m12,m13,m14,m21,m22,m23,m24,m31,m32,m33,m34,m41,m42,m43,m44]

normalize :: Floating a => [a] -> [a]
normalize = map . flip (/) . sqrt . foldr (+) 0 . map (**2) <*> id

cross [x,y,z] [x',y',z'] = [ y*z' - z*y', z*x' - x*z', x*y' - y*x' ]

dot :: Num a => [a] -> [a] -> a
dot = (foldr (+) 0 .) . zipWith (*)

identityLH :: [Float]
identityLH = take 16 . cycle $ [1,0,0,0,0]

lookAtLH position lookat up = let
    zAxis@[zax,zay,zaz] = (normalize . minus lookat) position
    xAxis@[xax,xay,xaz] = (normalize . cross up) zAxis
    yAxis@[yax,yay,yaz] = cross zAxis xAxis
    in
    [ xax, yax, zax, 0
    , xay, yay, zay, 0
    , xaz, yaz, zaz, 0
    , negate . dot xAxis $ position, negate . dot yAxis $ position, negate . dot zAxis $ position, 1 ]

perspectiveFovLH fieldOfView aspect near depth = let
    rtfov = recip . tan . (*0.5) $ fieldOfView
    denom = depth - near
    in
    [ rtfov*recip aspect, 0, 0, 0
    , 0, rtfov, 0, 0
    , 0, 0, (depth+near)/denom, 1
    , 0, 0, negate $ depth*near/denom, 0 ]

yRotationLH angle =
    [ cos angle, 0, -sin angle, 0
    , 0, 1, 0, 0
    , sin angle, 0, cos angle, 0
    , 0, 0, 0, 1 ]

translationLH [x,y,z] =
    [ 1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , x, y, z, 1 ]

orthoGraphicLH width height near far =
    [ 2/width, 0, 0, 0
    , 0, 2/height, 0, 0
    , 0, 0, 2/(far-near), 0
    , 0, 0, (far+near)/(near-far), 1 ]
