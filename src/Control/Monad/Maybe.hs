module Control.Monad.Maybe
    ( MaybeT(..)
    , hoistMaybe
    , module Control.Monad.Trans
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return = pure
    m >>= g = MaybeT $ runMaybeT m >>= maybe (pure Nothing) (runMaybeT . g)

instance Monad m => Applicative (MaybeT m) where
    pure = MaybeT . pure . Just
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Monad m => Functor (MaybeT m) where
    fmap f m = m >>= pure . f

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO io = MaybeT $ Just <$> liftIO io

instance Monad m => MonadPlus (MaybeT m)

instance Monad m => Alternative (MaybeT m) where
    empty = MaybeT $ pure Nothing
    m1 <|> m2 = MaybeT $ runMaybeT m1 >>= \x -> runMaybeT m2 >>= \y -> pure $ x <|> y

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
