module FPS (FPS,initialize,getFPS) where

import Graphics.UI.GLFW

import Flow.Render

data FPS = FPS {
    getFPS :: Int,
    getCount :: Int,
    getStartTime :: Double }
    deriving (Eq,Show)

initialize = do
    Just time <- getTime
    return (True, FPS 0 0 time)

instance Render FPS where
    render fps = do
        let count = getCount fps + 1
        Just time <- getTime
        if time > getStartTime fps + 1
        then return (True, FPS count 0 time)
        else return (True, fps { getCount = count })
