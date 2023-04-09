module Modelist (Modelist(..),initialize) where

import Control.Arrow
import System.Random

data Modelist = Modelist { getInfo :: [ModelInfo] }
    deriving (Eq,Show)

type ModelInfo = ([Float],[Float])

initialize numodels = do
    models <- sequence $ replicate numodels newModel
    return (True,Modelist models)

newModel :: IO ModelInfo
newModel = do
    red <- randomRIO (0,1)
    grn <- randomRIO (0,1)
    blu <- randomRIO (0,1)
    x <- randomRIO (-10,10)
    y <- randomRIO (-10,10)
    z <- randomRIO (-10,10)
    return $ ([x,y,z+5],[red,grn,blu,1])
