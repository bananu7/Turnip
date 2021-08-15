module Turnip.Eval.GC where

import Turnip.Eval.Types
import Turnip.Eval.Util

import Control.Monad.State
import qualified Data.Set as Set
import Control.Lens ((^.))

data GcGrab = GcMarkedTable TableRef | GcMarkedFunction FunctionRef deriving (Show, Eq, Ord)

gc :: LuaM ()
gc = do
    gr <- getGlobalTableRef
    -- find things reachable from _G
    marked <- execStateT (mark (Table gr)) (Set.fromList [])
    -- purge everything that's not in marked set
    return ()

mark :: Monad m => Value -> StateT (Set.Set GcGrab) (LuaMT m) ()
mark (Table tr) = do
    modify $ Set.insert (GcMarkedTable tr)
    td <- lift $ getTableData tr
    mapM_ (\v -> mark v) (td ^. mapData)
mark (Function fr) = modify $ Set.insert (GcMarkedFunction fr)
mark _ = return ()