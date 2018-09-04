{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module InterpreterT (
    InterpreterT(..),
    runInterpreterT,
    MonadInterpreter(..),
    lookupVar,
    setVar
) where

import BasicPrelude
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.State.Strict (StateT, evalStateT)
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

-- Needed to avoid impredicative polymorphism problems
data StateEffect a = StateEffect (forall n. MonadState InterpreterState n => n a)

-- This is a rather indirect way of embedding a state action into the monad without coupling ourselves to the impl
class Monad m => MonadInterpreter m where
    liftState :: StateEffect a -> m a

instance Monad m => MonadInterpreter (InterpreterT m) where
    liftState (StateEffect f) = InterpreterT f

runInterpreterT :: MonadIO m
                => InterpreterT m a -> m a
runInterpreterT (InterpreterT s) = evalStateT s initialState where
    initialState = InterpreterState DMS.empty

-- Helper functions
lookupVar :: MonadInterpreter m
          => Identifier -> m (Maybe Literal)
lookupVar k = liftState
            $ StateEffect
            $ gets
            . preview
            $ vars
            . ix k

setVar :: MonadInterpreter m
       => Identifier -> Literal -> m ()
setVar k v = liftState $ StateEffect (vars %= DMS.insert k v)

