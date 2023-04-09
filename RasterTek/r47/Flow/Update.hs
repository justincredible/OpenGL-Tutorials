module Flow.Update where

import Flow.Parameters

class Update a where
    update :: a -> Parameters -> IO (Bool,a)
    update a _ = return (False,a)

instance Update a => Update (Maybe a) where
    update ma p = maybe (pure (True,Nothing)) (fmap (fmap Just) . flip update p) ma
