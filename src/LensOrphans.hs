{-# LANGUAGE TypeFamilies #-}

module LensOrphans () where

import BasicPrelude
import qualified Data.HashMap.Strict as DMS
import Lens.Micro.Internal (Index, IxValue, Ixed(..))


-- Orphan instances/type families for HashMap used by microlens
instance (Eq k, Hashable k) => Ixed (DMS.HashMap k a) where
    ix k f m = case DMS.lookup k m of
        Just v  -> map (\v' -> DMS.insert k v' m) (f v)
        Nothing -> pure m
    {-# INLINE ix #-}

type instance Index   (DMS.HashMap k v) = k
type instance IxValue (DMS.HashMap k v) = v

