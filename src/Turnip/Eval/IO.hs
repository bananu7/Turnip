{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.IO where

import Turnip.Eval.Types
import Control.Lens.Operators

emptyIoBuf :: IoBuf
emptyIoBuf = IoBuf (Buffer "" 0) (Buffer "" 0)

bufferPrint :: String -> LuaM ()
bufferPrint str = LuaMT $ do
    (ioBuf . ioBufOut) %= \(Buffer s n) -> Buffer (s ++ str) (n + length s)
