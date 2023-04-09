module Tree (Tree,Tree.initialize,treeTrunk,treeLeaves,treeTexture,Tree.getPosition,modelMatrix) where

import Graphics.GL
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flow.Shutdown
import Maths
import Model

data Tree = Tree {
    getPosition :: [GLfloat],
    getScale :: Float,
    getTrunk :: Model,
    getLeaves :: Model }
    deriving (Eq, Show)

initialize scale trunkFile trunkTex trunkUnit leafFile leafTex leafUnit = do
    (tscs,mtrunk) <- Model.initialize trunkFile trunkTex trunkUnit True
    (lscs,mleaves) <- Model.initialize leafFile leafTex leafUnit True
    
    return . (,) (tscs && lscs) $ mtrunk >>= \trunk ->
        mleaves >>= \leaves ->
            Just (Tree [0,0,0] scale trunk leaves)

instance Shutdown Tree where
    shutdown tree = do
        shutdown . getTrunk $ tree
        shutdown . getLeaves $ tree

treeTrunk = getTrunk
treeLeaves = getLeaves
treeTexture = modelTexture
modelMatrix tree = let
    scale = getScale tree
    position = Tree.getPosition tree
    in
    [ scale, 0, 0, 0
    , 0, scale, 0, 0
    , 0, 0, scale, 0
    , head position, head . tail $ position, last position, 1 ]
    
    
