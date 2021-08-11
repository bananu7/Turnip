module Main where

import Turnip.Repl
import Turnip.Eval (defaultCtx)

main :: IO ()
main = do
    let ctx = defaultCtx
    ctx' <- handleCommandLine ctx
    repl ctx'
