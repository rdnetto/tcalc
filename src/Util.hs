module Util where

import BasicPrelude
import Lens.Micro (_Left, over)


mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft = over _Left
