{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.GC (gc) where

import Turnip.Eval.Types
import Turnip.Eval.Util

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Lens ((^.), (%=))

type Grabs = (Set.Set TableRef, Set.Set FunctionRef)

gc :: LuaM ()
gc = do
    gr <- getGlobalTableRef
    -- find things reachable from _G
    marked <- execStateT (mark (Table gr)) (Set.fromList [], Set.fromList [])
    -- purge everything that's not in marked set
    sweep marked

addTableRef :: TableRef -> Grabs -> Grabs
addTableRef tr (t, f) = (Set.insert tr t, f)

addFunctionRef :: FunctionRef -> Grabs -> Grabs
addFunctionRef fr (t, f) = (t, Set.insert fr f)

mark :: Monad m => Value -> StateT Grabs (LuaMT m) ()
mark (Table tr) = do
    trs <- fst <$> get

    if tr `Set.notMember` trs then do
        modify $ addTableRef tr
        td <- lift $ getTableData tr
        mapM_ (\v -> mark v) (td ^. mapData)
    else
        return ()

mark (Function fr) = modify $ addFunctionRef fr
mark _ = return ()

sweep :: Grabs -> LuaM ()
sweep (trs, frs) = do
    tables    %= Map.filterWithKey (\tr _ -> tr `Set.member` trs)
    functions %= Map.filterWithKey (\fr _ -> fr `Set.member` frs)
    return ()