module Frustum where

import Maths

data Frustum = Frustum { getPlanes :: [[Float]] }
    deriving (Eq,Show)

constructFrustum scrdepth projMx viewMx = let
    zmin = -(projMx!!11)/(projMx!!10)
    r = scrdepth/(scrdepth - zmin)
    [
        [m11,m12,m13,m14],
        [m21,m22,m23,m24],
        [m31,m32,m33,m34],
        [m41,m42,m43,m44] ] = flip matmult4 (unconcat viewMx) $
        updateMx 3 2 (-r*zmin) (updateMx 2 2 r (unconcat projMx))
    in
    [
        normalize [m14 + m11, m24 + m21, m34 + m31] ++ [m44 + m41], -- left
        normalize [m14 - m11, m24 - m21, m34 - m31] ++ [m44 - m41], -- right
        normalize [m14 + m12, m24 + m22, m34 + m32] ++ [m44 + m42], -- bottom
        normalize [m14 - m12, m24 - m22, m34 - m32] ++ [m44 - m42], -- top
        normalize [m14 + m13, m24 + m23, m34 + m33] ++ [m44 - m43], -- near
        normalize [m14 - m13, m24 - m23, m34 - m33] ++ [m44 + m43]  -- far
    ]
    where
unconcat xs = let (prefix,suffix) = splitAt 4 xs in
    if null suffix
    then [prefix]
    else prefix:unconcat suffix

updateMx r c v mx = let
    (prefix,xs:suffix) = splitAt c mx
    (start,x:finish) = splitAt r xs
    in
    prefix ++ (start ++ v:finish):suffix

planeDotCoord [x,y,z] [a,b,c,d] = a*x + b*y + c*z + d

checkPoint point frustum = all ((>= 0).planeDotCoord point) frustum

checkCube x y z s = checkRectangle x y z s s s

checkSphere center radius frustum = all ((>= -radius).planeDotCoord center) frustum

checkRectangle x y z sx sy sz frustum = and . map (flip any corners.((>= 0). ).flip planeDotCoord) $ frustum
    where
    corners = [
        [x-sx,y-sy,z-sz],
        [x-sx,y-sy,z+sz],
        [x-sx,y+sy,z-sz],
        [x-sx,y+sy,z+sz],
        [x+sx,y-sy,z-sz],
        [x+sx,y-sy,z+sz],
        [x+sx,y+sy,z-sz],
        [x+sx,y+sy,z+sz] ]
