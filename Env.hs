module Env where

import LuaAS
import qualified Data.Map as M

-- Environment is empty to start
-- Plannig to keep an associative list, where the pairs are bindings of Identifier and Value

env = M.empty

traverse ast =
    take 5 ast 

--addBinding :: Block -> Map
--addBinding     
