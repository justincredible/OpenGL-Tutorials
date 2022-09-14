module Particle where

import Data.List
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import System.Random

import Maths

data Particle = Particle {
    getPosSize :: [GLfloat],
    getColour :: [GLubyte],
    getSpeed :: [GLfloat],
    getLife :: GLfloat,
    getCameraDistance :: GLfloat }
    deriving (Eq, Show)

instance Ord Particle where
    compare = (. getCameraDistance) . flip compare . getCameraDistance

addNewParticles count maxSize particles = sequence (take count . repeat $ newParticle) >>= return . anp 0 particles
    where
    anp i ps [] = ps
    anp i [] ns = take (fromIntegral maxSize-i) ns
    anp i (p:ps) (n:ns)
        | getLife p < 0 = n:anp (i+1) ps ns
        | otherwise = p:anp (i+1) ps (n:ns)
    newParticle = do
        size <- randomRIO (-0.5,0.4995)
        
        dirx <- randomRIO (-1,0.999)
        diry <- randomRIO (-1,0.999)
        dirz <- randomRIO (-1,0.999)
        
        red <- randomRIO (0,255)
        grn <- randomRIO (0,255)
        blu <- randomRIO (0,255)
        alf <- randomRIO (0,85)
        
        return $ Particle
            [0,0,-20,size]
            [red,grn,blu,alf]
            (add [0,10,0] $ map (* 1.5) [dirx,diry,dirz])
            5 (-1)

simulateParticles delta camerapos particles = sp 0 particles id
    where
    sp c [] dp = (c,sort $ dp [])
    sp c (p:ps) dp = case (getLife p > 0, getLife p - delta > 0) of
        (False,_) -> let p' = p { getLife = -1, getCameraDistance = -1 } in
            sp c ps (dp.(p':))
        (_,False) -> let p' = p { getLife = -1, getCameraDistance = -1 } in
            sp (c+1) ps (dp.(p':))
        _ -> let
            life' = getLife p - delta
            speed' = add (getSpeed p) [0,-9.81*0.5*delta,0]
            posSize' = add (getPosSize p) (map (* delta) speed' ++ [0])
            p' = p {
                getLife = life',
                getSpeed = speed',
                getPosSize = posSize',
                getCameraDistance = dot <*> id $ minus (take 3 posSize') camerapos }
            in
            sp (c+1) ps (dp.(p':))
