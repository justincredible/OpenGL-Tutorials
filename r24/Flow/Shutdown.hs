module Flow.Shutdown where

class Shutdown a where
    shutdown :: a -> IO ()

instance Shutdown a => Shutdown (Maybe a) where
    shutdown = maybe (pure ()) shutdown
