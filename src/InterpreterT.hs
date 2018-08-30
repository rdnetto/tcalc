{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module InterpreterT (
    InterpreterT,
    runInterpreterT,
    lookupVar,
    setVar,
    incrementLineNo
) where

import BasicPrelude
import Control.Monad.State.Strict (StateT, evalStateT, gets)
import qualified Data.HashMap.Strict as DMS
import Lens.Micro (ix)
import Lens.Micro.Extras (preview)
import Lens.Micro.Mtl ((%=), (+=))
import Lens.Micro.TH (makeLenses)

import LensOrphans ()
import Parser.Identifier
import Types


-- Stores variables and their values
type VariableStore = DMS.HashMap Identifier Literal

data InterpreterState = InterpreterState {
    _vars :: VariableStore,
    _lineNo :: Int
} deriving (Eq, Show)
makeLenses ''InterpreterState

-- Monad transformer providing ability to execute parsed statements
newtype InterpreterT m a = InterpreterT (StateT InterpreterState m a)

runInterpreterT :: MonadIO m => InterpreterT m a -> m a
runInterpreterT (InterpreterT s) = evalStateT s initialState where
    initialState = InterpreterState DMS.empty 0

-- Helper functions
lookupVar :: Monad m => Identifier -> InterpreterT m (Maybe Literal)
lookupVar k = InterpreterT
            . gets
            . preview
            $ vars . ix k

setVar :: Monad m => Identifier -> Literal -> InterpreterT m ()
setVar k v = InterpreterT (vars %= DMS.insert k v)

incrementLineNo :: Monad m => InterpreterT m ()
incrementLineNo = InterpreterT (lineNo += 1)

