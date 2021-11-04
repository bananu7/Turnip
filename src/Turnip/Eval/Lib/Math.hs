{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib.Math where

import Turnip.Eval.TH

deg :: Floating a => a -> a
deg x = x / pi * 180

$(do
    entries <- sequence [
        -- math
         entry (Sig [NumberT] NumberT) "abs" 'abs
        ,entry (Sig [NumberT] NumberT) "acos" 'acos
        ,entry (Sig [NumberT] NumberT) "asin" 'asin
        ,entry (Sig [NumberT] NumberT) "atan" 'atan
        ,entry (Sig [NumberT, NumberT] NumberT) "atan2" 'atan2
        --,entry (Sig [NumberT] NumberT) "ceil" 'ceiling
        ,entry (Sig [NumberT] NumberT) "cos" 'cos
        ,entry (Sig [NumberT] NumberT) "cosh" 'cosh
        ,entry (Sig [NumberT] NumberT) "deg" 'deg
        ,entry (Sig [NumberT] NumberT) "exp" 'exp
        --,entry (Sig [NumberT] NumberT) "floor" 'floor
        --,entry (Sig [NumberT] NumberT) "fmod" 'fmod
        --,entry (Sig [NumberT] NumberT) "frexp" 'frexp
        ,entry (Sig [NumberT] NumberT) "sin" 'sin
        ]

    temps <- genDecls entries
    loadLib <- genLibLoadFunction "math" entries

    return $ temps ++ loadLib
 )
