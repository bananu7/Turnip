{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval.Lib (loadBaseLibrary) where

import Eval.Types
import Eval.TH
import Eval.Util

-- math helpers
deg x = x / pi * 180

$(do
    entries <- sequence [
        entry (Sig [NumberT, NumberT] NumberT) "+" '(+)
        ,entry (Sig [NumberT, NumberT] NumberT) "-" '(-)
        ,entry (Sig [NumberT, NumberT] NumberT) "*" '(*)
        ,entry (Sig [NumberT, NumberT] NumberT) "/" '(/)
        -- math
        ,entry (Sig [NumberT] NumberT) "math.abs" 'abs
        ,entry (Sig [NumberT] NumberT) "math.acos" 'acos
        ,entry (Sig [NumberT] NumberT) "math.asin" 'asin
        ,entry (Sig [NumberT] NumberT) "math.atan" 'asin
        ,entry (Sig [NumberT, NumberT] NumberT) "math.atan2" 'atan2
        --,entry (Sig [NumberT] NumberT) "math.ceil" 'ceiling
        ,entry (Sig [NumberT] NumberT) "math.cos" 'cos
        ,entry (Sig [NumberT] NumberT) "math.cosh" 'cosh
        ,entry (Sig [NumberT] NumberT) "math.deg" 'deg
        ,entry (Sig [NumberT] NumberT) "math.exp" 'exp
        --,entry (Sig [NumberT] NumberT) "math.floor" 'floor
        --,entry (Sig [NumberT] NumberT) "math.fmod" 'fmod
        --,entry (Sig [NumberT] NumberT) "math.frexp" 'frexp
        ]

    temps <- genDecls entries
    loadLib <- genLibLoadFunction entries

    return $ temps ++ loadLib
 )

