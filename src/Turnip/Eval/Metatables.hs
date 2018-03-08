{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Metatables where

import Turnip.Eval.Types
import Turnip.Eval.Util (getTableData)
import Control.Lens

getMetatable :: Value -> LuaM (Maybe TableRef)
getMetatable (Table tr) = (^. metatable) <$> getTableData tr
getMetatable _ = return Nothing

setMetatable :: TableRef -> Maybe TableRef -> LuaM ()
setMetatable tr mtr = LuaMT $ tables . at tr . traversed . metatable .= mtr

getMetaFunction :: String -> Value -> LuaM (Maybe FunctionRef)
getMetaFunction fstr v = do
    mtr <- getMetatable v
    case mtr of
        Just tr -> do
            f <- (^. mapData . at (Str fstr)) <$> getTableData tr
            case f of
                Just (Function fr) -> return $ Just fr
                _                  -> return Nothing
        Nothing -> return Nothing

