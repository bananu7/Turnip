{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval.Lib (loadBaseLibrary) where

import Eval.Types
import Eval.TH
import Eval.Util

$(do
    entries <- sequence [
            entry (Sig [NumberT, NumberT] NumberT) "+" '(+),
            entry (Sig [NumberT, NumberT] NumberT) "-" '(-)
            ]

    temps <- genDecls entries
    loadLib <- genLibLoadFunction entries

    return $ temps ++ loadLib
 )

