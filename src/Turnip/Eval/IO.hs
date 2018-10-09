{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.IO where

import Turnip.Eval.Types
import Control.Lens.Operators

emptyIoBuf :: IoBuf
emptyIoBuf = IoBuf (Buffer "" 0) (Buffer "" 0)

bufferPrint :: String -> LuaM ()
bufferPrint str = LuaMT $ do
    (ioBuf . ioBufOut) %= \(Buffer s n) -> Buffer (s ++ str) (n + length s)

flushOutBuffer :: Context -> (String, Context)
flushOutBuffer c = 
    let (Buffer s _) = c ^. (ioBuf . ioBufOut) in
    let c' = c & (ioBuf . ioBufOut) .~ (Buffer "" 0) in
    (s, c')
    
