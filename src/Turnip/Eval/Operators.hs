{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Operators where

import Turnip.Eval.Types
import Turnip.Eval.Metatables
import Turnip.Eval.Util
import Turnip.AST (BinaryOperator(..), UnaryOperator(..))

type BinaryOperatorImpl = Value -> Value -> LuaM [Value]
type UnaryOperatorImpl = Value -> LuaM [Value]

binaryOperatorCall :: BinaryOperator -> Value -> Value -> LuaM [Value]
binaryOperatorCall OpPlus a b = luaplus a b
binaryOperatorCall OpMinus a b = luaminus a b
binaryOperatorCall OpMult a b = luamult a b
binaryOperatorCall OpDivide a b = luadiv a b
binaryOperatorCall OpConcat a b = luaconcat a b

binaryOperatorCall op _ _ = vmErrorStr $ "Sorry, (" ++ op ++ ") operator is not implemented yet."

unaryOperatorCall :: UnaryOperator -> Value -> LuaM [Value]
unaryOperatorCall OpUnaryMinus a = luaunaryminus a
unaryOperatorCall OpConcat a = luaconcat a

luaplus :: BinaryOperatorImpl
luaplus (Number a) (Number b) = return $ [Number (a + b)]
luaplus a b = luametaop "__add" [a,b]

luamult :: BinaryOperatorImpl
luamult (Number a) (Number b) = return $ [Number (a * b)]
luamult a b = luametaop "__mult" [a,b]

luadiv :: BinaryOperatorImpl
luadiv (Number a) (Number b) = return $ [Number (a / b)]
luadiv a b = luametaop "__div" [a,b]

luaminus :: BinaryOperatorImpl
luaminus (Number a) = return $ [Number (-a)] --unary negate
luaminus a = luametaop "__unm" [a]

luaunaryminus :: UnaryOperatorImpl
luaunaryminus (Number a) (Number b) = return $ [Number (a - b)]
luaunaryminus a b = luametaop "__sub" [a,b]

luaconcat :: BinaryOperatorImpl
luaconcat (Str a) (Str b) = return [Str $ a ++ b]
luaconcat a b = luametaop "__concat" [a,b]

lualen :: UnaryOperatorImpl
lualen (Str a) = return [Number . fromIntegral $ length a]
lualen (Table tr) = do
    hasMetaLen <- isJust <$> getMetaFunction "__len" (Table tr)
    if hasMetaLen
        then luametaop "__len" [Table tr]
        else do
            (TableData td _) <- getTableData tr
            case lookupMax td of
                Just (Number x, _) -> return [Number x]
                _ -> return [Number 0]

lualen Nil = throwErrorStr "Attempt to get length of a nil value"
lualen a = luametaop "__len" [a]
