module Timer (Timer,initialize,getTiming) where

import Data.Int
import System.Win32.Time

import Flow.Render

data Timer = Timer {
    getFrequency :: Integer,
    getTicksPerMs :: Float,
    getStartTime :: Integer,
    getTiming :: Float }
    deriving (Eq,Show)

initialize = do
    frequency <- queryPerformanceFrequency
    
    if frequency == 0
    then return (False,Timer 0 0 0 0)
    else do
        startime <- queryPerformanceCounter
        return (True, Timer frequency (fromIntegral frequency/1000) startime 0)
        
instance Render Timer where
    render timer = do
        currentime <- queryPerformanceCounter
        let timediff = fromIntegral $ currentime - getStartTime timer
        return (True, timer { getTiming = timediff/getTicksPerMs timer, getStartTime = currentime })
