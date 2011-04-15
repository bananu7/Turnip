module Env where

import LuaAS


-- 
--data Env = Env { syms : [(Sym, Allocation)], insts : [String], funcs : [(String, Signature)] }



--foldl compile initialEnv stmts

--compile (Env syms insts) (Do blk) = let (Env _ i) = foldl compile (Env syms []) (statements blk)  in  Env 
--               syms (insts ++ i)


statements :: Block -> [Stmt]
statements (Block s) = s



--traverse :: M.Map -> Stmt -> M.Map
--traverse env stmt = M.insert 1 stmt env 
--addBinding :: Block -> Map
--addBinding     
