{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Service where



class Monad m => Service m (e :: * -> *) where
    run :: e a -> m a

