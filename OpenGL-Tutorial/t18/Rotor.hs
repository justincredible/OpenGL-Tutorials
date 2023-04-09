module Rotor (Rotor, fromAngleAxis, rotationMatrix3, rotationMatrix4, lookAt, rotateTowards) where

import Data.Foldable
import Data.Ord
import Graphics.GL

import Maths

data Rotor a = Rotor a a a a deriving (Eq, Show)

instance Functor Rotor where
    fmap f (Rotor x y z w) = Rotor (f x) (f y) (f z) (f w)

instance Foldable Rotor where
    foldMap f (Rotor x y z w) = f x <> f y <> f z <> f w

instance Num a => Num (Rotor a) where
    (Rotor x y z w) + (Rotor x' y' z' w') = Rotor (x+x') (y+y') (z+z') (w+w')
    (Rotor x y z w) * (Rotor x' y' z' w') = Rotor
        (x*w' + w*x' - y*z' + z*y')
        (y*w' + w*y' - z*x' + x*z')
        (z*w' + w*z' - x*y' + y*x')
        (w*w' - x*x'- y*y' - z*z')
    signum (Rotor x y z w) = Rotor 0 0 0 (signum $ x*y*z)
    abs r = r*signum r
    negate r = fmap negate r
    fromInteger _ = Rotor 0 0 0 1

conjugate (Rotor x y z w) = Rotor (-x) (-y) (-z) w

inverse = fmap . flip (/) . (inner <*> id) <*> conjugate

magnitude (Rotor x y z w) = sqrt $ x*x + y*y + z*z + w*w

normalize :: Floating a => Rotor a -> Rotor a
normalize = fmap . flip (/) . magnitude <*> id

inner :: Floating a => Rotor a -> Rotor a -> a
inner r1 r2 = foldr (+) 0 $ zipWith (*) (toList r1) (toList r2)

{-rotate [x,y,z,xyz] r = let
    Rotor yz zx xy w  normalize r
    -- step 1: g = v*r
    x' = w*x - y*xy + z*zx - xyz*yz
    y' = w*y - z*yz + x*xy - xyz*zx
    z' = w*z - x*zx + y*yz - xyz*xy
    xyz' = w*xyz + x*yz + y*zx + z*xy
    -- step 2: v' = conjugate r*g
    x'' = w*x' - y'*xy + z'*zx + xyz'*yz
    y'' = w*y' - z'*yz + x'*xy + xyz'*zx
    z'' = w*z' - x'*zx + y'*yz + xyz'*xy
    xyz'' = w*xyz' - x'*yz - y'*zx - z'*xy
    in
    [x'',y'',z'',xyz'']-}
rotate [x,y,z] r = rotate [x,y,z,0] r
rotate [x,y,z,xyz] r = let
    Rotor yz zx xy w = normalize r
    v = [x,y,z]
    p = [yz,zx,xy]
    vxp = cross v p
    in
    zipWith3 (((+) .).(+)) v (map (* (-2*w)) vxp) (add <*> id $ cross vxp p) ++ [xyz]

--rotationMatrix3 :: Rotor GLfloat -> [[GLfloat]]
rotationMatrix3 (Rotor x y z w) = [
    [1 - 2*(y*y + z*z), 2*(x*y + z*w), 2*(x*z - y*w)],
    [2*(x*y - z*w), 1 - 2*(z*z + x*x), 2*(y*z + x*w)],
    [2*(x*z + y*w), 2*(y*z - x*w), 1 - 2*(x*x + y*y)] ]

rotationMatrix4 :: Rotor GLfloat -> [[GLfloat]]
rotationMatrix4 = (++ [[0,0,0,1]]) . map (++ [0]) . rotationMatrix3

fromAngleAxis :: Floating a => a -> [a] -> Rotor a
fromAngleAxis = (normalize .) . angleAxis

angleAxis angle axis = let
    w = cos (angle/2)
    [x,y,z] = map (* sin (angle/2)) axis
    in
    Rotor x y z w

rotationBetween start dest = let
    s = normalise start
    d = normalise dest
    costheta = dot s d
    in
    if costheta < -1 + 0.001
    then if (dot <*> id) (cross [0,0,1] s) < 0.01
        then angleAxis pi $ cross [1,0,0] s
        else angleAxis pi $ cross [0,0,1] s
    else let
        w2 = sqrt $ 2*(1 + costheta)
        [x,y,z] = map (/w2) $ cross s d
        in
        Rotor x y z (w2*0.5)
    where

lookAt direction up =
    if (dot <*> id) direction < 0.0001
    then fromInteger 0
    else let
        right = cross direction up
        desiredUp = cross right direction
        in
        normalize $ rotationBetween [0,0,1] direction * rotationBetween [0,1,0] desiredUp

rotateTowards r1 r2 maxAngle = let
    costheta = inner r1 r2
    angle = acos costheta
    in
    case (maxAngle < 0.001, costheta > 0.9999 || angle < maxAngle) of
        (True,_) -> r1
        (_,True) -> r2
        _ -> let
            r1' = if costheta < 0 then negate r1 else r1
            t = maxAngle/angle
            in
            normalize $ fmap (* (sin (1-t)*maxAngle/sin maxAngle)) r1' + fmap (* (sin (t*maxAngle)/sin maxAngle)) r2
