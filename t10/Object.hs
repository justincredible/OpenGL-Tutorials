module Object (loadObj,indexVBO) where

import qualified Data.Map as Map
import Graphics.GL

loadObj :: FilePath -> IO [[[GLfloat]]]
loadObj = fmap (
        process
        . compile
        . filter include
        .lines )
    . readFile
    where
    include ('v':_) = True
    include ('f':_) = True
    include _ = False
    compile = compile' id id id id
    compile' dv dt dn di [] = (dv [], dt [], dn [], di [])
    compile' dv dt dn di (x:xs)
        | take 2 x == "v " = let vertex = map read . tail . split ' ' $ x in
            compile' (dv.(vertex:)) dt dn di xs
        | take 2 x == "vt" = let [u,v] = map read . tail . split ' ' $ x in
            compile' dv (dt.([u,-v]:)) dn di xs
        | take 2 x == "vn" = let normal = map read . tail . split ' ' $ x in
            compile' dv dt (dn.(normal:)) di xs
        | take 2 x == "f " = let [i1,i2,i3] = map (map read.split '/') . tail . split ' ' $ x in
            compile' dv dt dn (di.(i1:).(i2:).(i3:)) xs
        | otherwise = error "Incorrect file format:\n-v vt vn f\n\n"
    split c s = let (chunk, rest) = break (== c) s in
        case rest of
            [] -> [chunk]
            _:rest -> chunk : split c rest
    process (vertices,uvcoords,normals,indices) = process' vertices uvcoords normals id id id indices
    process' xs ys zs dx dy dz [] = [dx [], dy [], dz []]
    process' xs ys zs dx dy dz ([x,y,z]:indices) =
        process' xs ys zs
            (dx.((head . drop (x-1)) xs:))
            (dy.((head . drop (y-1)) ys:))
            (dz.((head . drop (z-1)) zs:))
            indices

indexVBO :: [[[GLfloat]]] -> [[[GLfloat]]]
indexVBO [positions, coordinates, normals] = process Map.empty 0 positions coordinates normals id id id id
    where
    {-process :: Map.Map ([Float],[Float],[Float]) Float -> Int ->
        [[Float]] -> [[Float]] -> [[Float]] ->
        ([[Float]] -> [[Float]]) -> ([[Float]] -> [[Float]]) -> ([[Float]] -> [[Float]]) -> ([[Float]] -> [[Float]]) -> 
        [[[Float]]]-}
    process im i [] [] [] dp dc dn di = [dp [], dc [], dn [], di []]
    process im i (x:xs) (y:ys) (z:zs) dp dc dn di
        | Map.member (x,y,z) im =
            process im i xs ys zs dp dc dn $ maybe di ((di.).(:).(:[])) (Map.lookup (x,y,z) im)
        | otherwise =
            process (Map.insert (x,y,z) (fromIntegral i) im) (i+1) xs ys zs (dp.(x:)) (dc.(y:)) (dn.(z:)) (di.([fromIntegral i]:))
