module Object (loadObj, indexVBO, computeTangentBasis, indexVBO_TBN) where

import Data.Bifunctor
import Data.List
import qualified Data.Map as Map
import Graphics.GL

import Maths

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

computeTangentBasis :: [[[GLfloat]]] -> [[[GLfloat]]]
computeTangentBasis [positions, coordinates, normals] = process positions coordinates normals id id
    where
    --process :: [[GLfloat]] -> [[GLfloat]] -> [[GLfloat]] -> ([[GLfloat]] -> [[GLfloat]]) -> ([[GLfloat]] -> [[GLfloat]]) -> [[[GLfloat]]]
    process [] [] [] dt db = [positions, coordinates, normals, dt [], db []]
    process (v0:v1:v2:vs) (u0:u1:u2:us) (n0:n1:n2:ns) dt db = let
        dv1 = minus v1 v0
        dv2 = minus v2 v0
        du1 = minus u1 u0
        du2 = minus u2 u0
        r = 1/(head du1*last du2 - last du1*head du2)
        t = map (*r) (minus (map (*last du2) dv1) (map (*last du1) dv2));
        b = map (*r) (minus (map (*head du1) dv2) (map (*head du2) dv1));
        t0 = fixhand (dot (cross n0 t) b) . normalize . minus t $ map (*dot n0 t) n0
        t1 = fixhand (dot (cross n1 t) b) . normalize . minus t $ map (*dot n1 t) n1
        t2 = fixhand (dot (cross n2 t) b) . normalize . minus t $ map (*dot n2 t) n2
        in
        process vs us ns (dt.(t0:).(t1:).(t2:)) (db.(b:).(b:).(b:))
    fixhand dc = if dc < 0 then map (*(-1)) else id

indexVBO_TBN :: [[[GLfloat]]] -> [[[GLfloat]]]
indexVBO_TBN [positions, coordinates, normals, tangents, bitangents] =
    process Map.empty 0 positions coordinates normals tangents bitangents id id id id
    where
    process itbm i [] [] [] [] [] dp dc dn di =
        [dp [], dc [], dn [], map (fst . snd) . sortOn fst . Map.elems $ itbm, map (snd . snd) . sortOn fst . Map.elems $ itbm, di []]
    process itbm i (p:ps) (c:cs) (n:ns) (t:ts) (b:bs) dp dc dn di
        | Map.member (p,c,n) itbm = let
            itbm' = Map.adjust (fmap $ bimap (add t) (add b)) (p,c,n) itbm
            in
            process itbm' i ps cs ns ts bs dp dc dn $
                maybe di ((di.).(:).(:[]).fst) (Map.lookup (p,c,n) itbm)
        | otherwise =
            process (Map.insert (p,c,n) (fromIntegral i,(t,b)) itbm) (i+1) ps cs ns ts bs (dp.(p:)) (dc.(c:)) (dn.(n:)) (di.([fromIntegral i]:))
