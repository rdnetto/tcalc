{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module InterpreterT (
    InterpreterT(..),
    runInterpreterT,
    MonadInterpreter(..),
    lookupVar,
    setVar
) where

import BasicPrelude
import Control.Monad.State.Strict (StateT, evalStateT, gets)
import Control.Monad.Trans.Class (MonadTrans(..))
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
} deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

class Monad m => MonadInterpreter m where
    liftInterpreter :: StateT InterpreterState m a -> InterpreterT m a

instance Monad m => MonadInterpreter (InterpreterT m) where
    liftInterpreter = InterpreterT

runInterpreterT :: MonadIO m => InterpreterT m a -> m a
runInterpreterT (InterpreterT s) = evalStateT s initialState where
    initialState = InterpreterState DMS.empty


-- Helper functions
lookupVar :: MonadInterpreter m => Identifier -> InterpreterT m (Maybe Literal)
lookupVar k = liftInterpreter
            . gets
            . preview
            $ vars . ix k

setVar :: MonadInterpreter m => Identifier -> Literal -> InterpreterT m ()
setVar k v = liftInterpreter (vars %= DMS.insert k v)

