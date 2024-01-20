module Control.Monad.List
    ( ListT(..)
    , hoistList
    , module Control.Monad.Trans
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative

newtype ListT m a = ListT { runListT :: m [a] }

instance Monad m => Monad (ListT m) where
    return = pure
    m >>= g = ListT $ runListT m >>= \xs -> concat <$> mapM (runListT . g) xs

instance Monad m => Applicative (ListT m) where
    pure x = ListT $ pure [x]
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Monad m => Functor (ListT m) where
    fmap f m = m >>= pure . f

instance MonadIO m => MonadIO (ListT m) where
    liftIO io = ListT $ pure <$> liftIO io

instance Monad m => MonadPlus (ListT m)

instance Monad m => Alternative (ListT m) where
    empty = ListT $ pure []
    m1 <|> m2 = ListT $ runListT m1 >>= \x -> runListT m2 >>= \y -> pure $ x <|> y

hoistList :: Monad m => [a] -> ListT m a
hoistList = ListT . pure
