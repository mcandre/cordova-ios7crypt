module Random where

import Language.Fay.FFI
import Language.Fay.Prelude

data Element
instance Foreign Element
instance Show (Element)

-- Monad returning a uniform random integer from [0,m)
random :: Int -> Fay Int
random = ffi "Math.floor(Math.random() * %1)"
