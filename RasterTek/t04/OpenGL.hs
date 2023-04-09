module OpenGL where

import Graphics.Rendering.OpenGL hiding (normalize)

add :: (Applicative f, Num a) => f a -> f a -> f a
add = (<*>) . fmap (+)
minus a b = add a $ fmap negate b

multiply3 (Vector3 a b c) [Vector3 m11 m21 m31,Vector3 m12 m22 m32,Vector3 m13 m23 m33] =
    Vector3 (a*m11 + b*m12 + c*m13) (a*m21 + b*m22 + c*m23) (a*m31 + b*m32 + c*m33)

normalize :: (Applicative f, Foldable f, Floating b) => f b -> f b
normalize = (<*>) . pure . flip (/) . sqrt . foldr (+) 0 . fmap (**2) <*> id
--normalize vec3 = (pure . flip (/) . sqrt . foldr (+) 0 . fmap (**2)) vec3 <*> vec3

cross (Vector3 x y z) (Vector3 x' y' z') = Vector3 (y*z' - z*y') (z*x' - x*z') (x*y' - y*x')

dot :: (Applicative f, Foldable f, Num a) => f a -> f a -> a
dot = (foldr (+) 0 .) . (<*>) . fmap (*)

identityLH :: IO (GLmatrix GLfloat)
identityLH = do
    newMatrix ColumnMajor
    . take 16
    . cycle
    $ [1,0,0,0,0]

perspectiveFovLH :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO (GLmatrix GLfloat)
perspectiveFovLH fov aspect near depth = newMatrix ColumnMajor [
    recip . (*aspect) . tan $ fov*0.5, 0, 0, 0,
    0, recip . tan $ fov*0.5, 0, 0,
    0, 0, (depth+near)/(depth-near), 1,
    0, 0, negate $ depth*near/(depth-near), 0 ]
