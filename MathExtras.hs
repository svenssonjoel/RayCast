{-# LANGUAGE ForeignFunctionInterface #-}

module MathExtras where

import Data.Word

floor_ :: Float -> Word32
floor_ x = truncate (c_floor x)

foreign import ccall unsafe "math.h floorf"
    c_floor :: Float -> Float