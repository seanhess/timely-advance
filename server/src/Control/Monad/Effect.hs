{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Effect where



class Monad m => Effect m (e :: * -> *) where
    run :: e a -> m a

