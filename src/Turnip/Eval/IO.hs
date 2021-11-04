{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Turnip.Eval.IO where

import Turnip.Eval.Types

import Control.Monad.Trans (lift)
import Control.Lens (use)

ioOut :: Monad m => String -> LuaMT m ()
ioOut x = do
    cb :: String -> m () <- LuaMT $ use ioOutCb
    lift $ cb x

ioIn :: LuaM String
ioIn = do 
    cb :: m String <- LuaMT $ use ioInCb
    lift cb
