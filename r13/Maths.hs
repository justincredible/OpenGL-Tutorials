module Maths where

{- Maths:
    Linear Algebra functions for vectors and matrices
-}

add :: Num a => [a] -> [a] -> [a]
add = zipWith (+)

minus a b = add a $ map negate b

multiply3 [x,y,z] [[m11,m21,m31],[m12,m22,m32],[m13,m23,m33]] =
    [ x*m11 + y*m12 + z*m13, x*m21 + y*m22 + z*m23, x*m31 + y*m32 + z*m33 ]

normalize :: Floating a => [a] -> [a]
normalize = map . flip (/) . sqrt . foldr (+) 0 . map (**2) <*> id

cross [x,y,z] [x',y',z'] = [ y*z' - z*y', z*x' - x*z', x*y' - y*x' ]

dot :: Num a => [a] -> [a] -> a
dot = (foldr (+) 0 .) . zipWith (*)

identityLH :: [Float]
identityLH = take 16 . cycle $ [1,0,0,0,0]

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

translationLH x y z =
    [ 1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , x, y, z, 1 ]

orthoGraphicLH width height near far =
    [ 2/width, 0, 0, 0
    , 0, 2/height, 0, 0
    , 0, 0, 2/(near-far), 0
    , 0, 0, (far + near)/(near-far), 1 ]
