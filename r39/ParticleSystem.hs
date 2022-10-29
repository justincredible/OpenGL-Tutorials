module ParticleSystem (ParticleSystem,ParticleSystem.initialize,systemTexture) where

import Data.List
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import qualified GHC.Base as GHC
import Graphics.GL
import System.Random

import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update
import Texture

data ParticleSystem = ParticleSystem {
    getSystem :: System,
    getCurrentCount :: Int,
    getAccumTime :: GLfloat,
    getTexture :: Maybe Texture,
    getParticles :: [Particle],
    getIndexCount :: GLint,
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint }
    deriving (Eq,Show)

data System = System {
    getDeviation :: [GLfloat],
    getVelo :: GLfloat,
    getVeloVari :: GLfloat,
    getSize :: GLfloat,
    getParticlesPerSec :: Int,
    getMaxParticles :: Int }
    deriving (Eq,Show)

data Particle = Particle {
    getPosition :: [GLfloat],
    getColour :: [GLfloat],
    getVelocity :: GLfloat,
    getActive :: Bool }
    deriving (Eq,Show)

instance Ord Particle where
    compare = (. last . getPosition) . flip compare . last . getPosition

initialize texFile texUnit wrap = do
    let maxps = 5000
        vxsz = 36
    
    vao <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
    glBindVertexArray vao
    
    [vbo,ebo] <- allocaArray 2 $ (>>) . glGenBuffers 2 <*> peekArray 2

    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER (fromIntegral maxps*4*fromIntegral vxsz) nullPtr GL_DYNAMIC_DRAW
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE vxsz nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE vxsz $ bufferOffset (3*sizeOf(0::GLfloat))
    glVertexAttribPointer 2 4 GL_FLOAT GL_FALSE vxsz $ bufferOffset (5*sizeOf(0::GLfloat))
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
    withArray (indices (fromIntegral maxps) :: [GLushort]) $
        flip (glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ maxps*6*2)) GL_STATIC_DRAW
    
    sequence_ $ map glEnableVertexAttribArray [0..2]
    
    glBindVertexArray 0
    
    (success, texture) <- Texture.initialize texFile texUnit wrap
    return (success,Just $ ParticleSystem (System [0.5,0.1,2] 1 0.2 0.2 250 maxps) 0 0 texture [] 0 vao vbo ebo)
    where
    bufferOffset = plusPtr nullPtr . fromIntegral
indices = generate 0 . (4*)
generate n mx
    | n == mx = []
    | otherwise = n:(n+1):(n+2):(n+2):(n+1):(n+3):generate (n+4) mx

emitParticles frametime particles = do
    let accumtime = getAccumTime particles + frametime
        resettime = accumtime > 1000/(fromIntegral . getParticlesPerSec . getSystem $ particles)
        (livecnt,living) = ((,).length<*>id) . filter alive . getParticles $ particles
        capacity = (getMaxParticles . getSystem $ particles) - livecnt > 0
    
    if not resettime || not capacity
    then return (True, particles { getAccumTime = if resettime then 0 else accumtime })
    else do
        newparticle <- randomParticle
        let allparticles = merge [newparticle] living
            particlecnt = length allparticles
        return (True, particles {
            getParticles = allparticles,
            getCurrentCount = particlecnt })
    where
    alive = (&&) . getActive <*> (>= -3) . head . tail . getPosition
    merge xs [] = xs
    merge [] ys = ys
    merge xs@(x:xs') ys@(y:ys')
        | y < x = y:merge xs ys'
        | otherwise = x:merge xs' ys
    randomParticle = do
        rs <- sequence $ replicate 14 (randomIO :: IO Int)
        let randMax = fromIntegral $ GHC.maxInt
            position = zipWith (*) (getDeviation . getSystem $ particles) [
                fromIntegral (rs!!0 - rs!!1)/randMax,
                fromIntegral (rs!!2 - rs!!3)/randMax,
                fromIntegral (rs!!4 - rs!!5)/randMax ]
            velocity = (getVelo . getSystem $ particles) +
                (fromIntegral (rs!!6 - rs!!7)/randMax)*(getVeloVari . getSystem $ particles)
            colour = zipWith (+) [0.5,0.5,0.5] [
                fromIntegral (rs!!8 - rs!!9)/randMax,
                fromIntegral (rs!!10 - rs!!11)/randMax,
                fromIntegral (rs!!12 - rs!!13)/randMax ]
        return (Particle position colour velocity True)

instance Render ParticleSystem where
    render particles = do
        glBindVertexArray (getVertexArray particles)
        
        glDrawElements GL_TRIANGLES (getIndexCount particles) GL_UNSIGNED_SHORT nullPtr
        
        glBindVertexArray 0
        
        return (True,particles)

instance Update ParticleSystem where
    update particles (F time) = do
        let fallparticles = map (fall time) $ getParticles particles
        emitParticles time particles { getParticles = fallparticles }
    update particles None = do
        let vertices = concat . map (vertex (getSize . getSystem $ particles)) . getParticles $ particles
            vxcnt = 4*getCurrentCount particles
            idxcnt = 6*getCurrentCount particles
            
        glBindVertexArray (getVertexArray particles)
        withArray vertices $ glBufferSubData GL_ARRAY_BUFFER 0 (fromIntegral $ vxcnt*36)
        glBindVertexArray 0
        
        return (True,particles {
            getIndexCount = fromIntegral idxcnt })
    update particles _ = do
        putStrLn "Incorrect frame parameters."
        return (False,particles)

fall time particle = let
    lastY = head . tail . getPosition $ particle
    velocity = getVelocity particle
    position = [ head . getPosition $ particle, lastY - velocity*time*0.001, last . getPosition $ particle ]
    in
    particle { getPosition = position }

vertex size particle = [
    (head . getPosition) particle - size,
    (head . tail . getPosition) particle + size,
    last . getPosition $ particle,
    0,0,
    head . getColour $ particle,
    head . tail . getColour $ particle,
    last . getColour $ particle,
    1,
    (head . getPosition) particle - size,
    (head . tail . getPosition) particle - size,
    last . getPosition $ particle,
    0,1,
    head . getColour $ particle,
    head . tail . getColour $ particle,
    last . getColour $ particle,
    1,
    (head . getPosition) particle + size,
    (head . tail . getPosition) particle + size,
    last . getPosition $ particle,
    1,0,
    head . getColour $ particle,
    head . tail . getColour $ particle,
    last . getColour $ particle,
    1,
    (head . getPosition) particle + size,
    (head . tail . getPosition) particle - size,
    last . getPosition $ particle,
    1,1,
    head . getColour $ particle,
    head . tail . getColour $ particle,
    last . getColour $ particle,
    1 ]

instance Shutdown ParticleSystem where
    shutdown particles = do
        shutdown (getTexture particles)
        
        glBindVertexArray (getVertexArray particles)
        
        sequence_ $ map glDisableVertexAttribArray [0..2]
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        glBindBuffer GL_ARRAY_BUFFER 0
        withArray [getVertexBuffer particles,getIndexBuffer particles] $ glDeleteBuffers 2
        
        glBindVertexArray 0
        with (getVertexArray particles) $ glDeleteVertexArrays 1

systemTexture (ParticleSystem _ _ _ (Just texture) _ _ _ _ _) = fromIntegral . textureUnit $ texture
