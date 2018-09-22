{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Operators where

import Turnip.Eval.Types
import Turnip.Eval.Metatables
import Turnip.Eval.Util
import Turnip.AST (BinaryOperator(..), UnaryOperator(..))

import Data.Maybe (isJust)
import qualified Data.Map as Map (lookupMax)

type BinaryOperatorImpl = Value -> Value -> LuaM [Value]
type UnaryOperatorImpl = Value -> LuaM [Value]

binaryMetaOperatorCall :: BinaryOperator -> Value -> Value -> LuaM [Value]
binaryMetaOperatorCall OpPlus a b = luaplus a b
binaryMetaOperatorCall OpMinus a b = luaminus a b
binaryMetaOperatorCall OpMult a b = luamult a b
binaryMetaOperatorCall OpDivide a b = luadiv a b
binaryMetaOperatorCall OpConcat a b = luaconcat a b

binaryMetaOperatorCall op _ _ = vmErrorStr $ "Sorry, (" ++ show op ++ ") operator is not implemented yet."

unaryOperatorCall :: UnaryOperator -> Value -> LuaM [Value]
unaryOperatorCall OpUnaryMinus a = luaunaryminus a

{-
  https://www.lua.org/pil/13.1.html
  To choose a metamethod, Lua does the following:
    (1) If the first value has a metatable with an __add field, Lua uses this value as the metamethod,
        independently of the second value;
    (2) otherwise, if the second value has a metatable with an __add field, Lua uses this value as the metamethod;
    (3) otherwise, Lua raises an error.

    __add, __mul, __sub (for subtraction), __div (for division),
    __unm (for negation), and __pow
-}

binaryMetaOperator :: String -> NativeFunction
binaryMetaOperator fstr (a : b : _) = do
    maybeFn <- getMetaFunction fstr a
    case maybeFn of
        Just fra -> callRef fra [a,b]
        _ -> do
            maybeFnB <- getMetaFunction fstr b
            case maybeFnB of
                Just frb -> callRef frb [a,b]
                _ -> throwErrorStr $ "No metaop '" ++ fstr ++ "' on those two values"

binaryMetaOperator _ _ = vmErrorStr "Invalid binary metaop call"

unaryMetaOperator :: String -> NativeFunction
unaryMetaOperator fstr [a] = do
    maybeFn <- getMetaFunction fstr a
    case maybeFn of
        Just fr -> callRef fr [a]
        _ -> throwErrorStr $ "No metaop '" ++ fstr ++ "' on this value"

unaryMetaOperator _ _ = vmErrorStr "Invalid unary metaop call"

luaplus :: BinaryOperatorImpl
luaplus (Number a) (Number b) = return $ [Number (a + b)]
luaplus a b = binaryMetaOperator "__add" [a,b]

luamult :: BinaryOperatorImpl
luamult (Number a) (Number b) = return $ [Number (a * b)]
luamult a b = binaryMetaOperator "__mult" [a,b]

luadiv :: BinaryOperatorImpl
luadiv (Number a) (Number b) = return $ [Number (a / b)]
luadiv a b = binaryMetaOperator "__div" [a,b]

luaunaryminus :: UnaryOperatorImpl
luaunaryminus (Number a) = return $ [Number (-a)] --unary negate
luaunaryminus a = unaryMetaOperator "__unm" [a]

luaminus :: BinaryOperatorImpl
luaminus (Number a) (Number b) = return $ [Number (a - b)]
luaminus a b = binaryMetaOperator "__sub" [a,b]

luaconcat :: BinaryOperatorImpl
luaconcat (Str a) (Str b) = return [Str $ a ++ b]
luaconcat a b = binaryMetaOperator "__concat" [a,b]

lualen :: UnaryOperatorImpl
lualen (Str a) = return [Number . fromIntegral $ length a]
lualen (Table tr) = do
    hasMetaLen <- isJust <$> getMetaFunction "__len" (Table tr)
    if hasMetaLen
        then unaryMetaOperator "__len" [Table tr]
        else do
            (TableData td _) <- getTableData tr
            case Map.lookupMax td of
                Just (Number x, _) -> return [Number x]
                _ -> return [Number 0]

lualen Nil = throwErrorStr "Attempt to get length of a nil value"
lualen a = unaryMetaOperator "__len" [a]
