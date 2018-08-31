{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module InterpreterT (
    InterpreterT(..),
    runInterpreterT,
    lookupVar,
    setVar
) where

import BasicPrelude
import Control.Monad.State.Strict (StateT, evalStateT, gets)
import qualified Data.HashMap.Strict as DMS
import Lens.Micro (ix)
import Lens.Micro.Extras (preview)
import Lens.Micro.Mtl ((%=))
import Lens.Micro.TH (makeLenses)

import LensOrphans ()
import Parser.Identifier
import Types


-- Stores variables and their values
type VariableStore = DMS.HashMap Identifier Literal

data InterpreterState = InterpreterState {
    _vars :: VariableStore
} deriving (Eq, Show)
makeLenses ''InterpreterState

-- Monad transformer providing ability to execute parsed statements
newtype InterpreterT m a = InterpreterT {
    unInterpreterT :: StateT InterpreterState m a
}

instance Functor f => Functor (InterpreterT f) where
    fmap f (InterpreterT x) = InterpreterT $ fmap f x

instance (Applicative f, Monad f) => Applicative (InterpreterT f) where
    pure = InterpreterT . pure
    liftA2 f fa fb = InterpreterT
                   $ f
                   <$> unInterpreterT fa
                   <*> unInterpreterT fb

instance Monad m => Monad (InterpreterT m) where
    return = InterpreterT . return
    (>>=) (InterpreterT ma) f = InterpreterT $ ma >>= (unInterpreterT . f)

instance MonadIO m => MonadIO (InterpreterT m) where
    liftIO = InterpreterT . liftIO

runInterpreterT :: MonadIO m => InterpreterT m a -> m a
runInterpreterT (InterpreterT s) = evalStateT s initialState where
    initialState = InterpreterState DMS.empty

-- Helper functions
lookupVar :: Monad m => Identifier -> InterpreterT m (Maybe Literal)
lookupVar k = InterpreterT
            . gets
            . preview
            $ vars . ix k

setVar :: Monad m => Identifier -> Literal -> InterpreterT m ()
setVar k v = InterpreterT (vars %= DMS.insert k v)

