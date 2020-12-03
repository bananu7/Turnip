module Turnip.Eval.UtilNumbers
    (isInt
    ,toInt
    ,decimalDigits
    ,readNumberBase
    )
where

import Data.Char (ord)
import Turnip.Eval.Types (Value(..))

isInt :: Double -> Bool
isInt x = x == (fromIntegral ((floor :: Double -> Int) x))


toInt :: Double -> Maybe Int
toInt x = if isInt x then Just . floor $ x else Nothing


decimalDigits :: Double -> Maybe Int
decimalDigits x = if isInt x then Just 0 else Nothing


readNumberBase :: Int -> String -> Value
readNumberBase base s = maybe Nil (Number . fromIntegral) maybeVal
    where
        maybeVal = foldl (accumDigit base) (Just 0) . zip [0..] . reverse $ s

        accumDigit :: Int -> Maybe Int -> (Int, Char) -> Maybe Int
        accumDigit base mval (n, digit) = do
            val <- mval
            di <- digitToIntBase base digit
            return $ val + di * base ^ n


digitToIntBase :: Int -> Char -> Maybe Int
digitToIntBase base c
  | (fromIntegral dec::Word) <= 9 = Just dec
  | (fromIntegral alphal::Word) <= nonnumeric = Just $ alphal + 10
  | (fromIntegral alphau::Word) <= nonnumeric = Just $ alphau + 10
  | otherwise = Nothing
  where
    dec = ord c - ord '0'
    alphal = ord c - ord 'a'
    alphau = ord c - ord 'A'

    nonnumeric :: Word
    nonnumeric = fromIntegral base - 10 - 1
