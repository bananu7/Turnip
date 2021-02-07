{-# LANGUAGE TupleSections #-}

module Turnip.Eval.UtilNumbers
    (isInt
    ,toInt
    ,decimalDigits
    ,readNumberBase
    )
where

import Data.Char (ord)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Turnip.Eval.Types (Value(..))
import Text.ParserCombinators.Parsec

isInt :: Double -> Bool
isInt x = x == (fromIntegral ((floor :: Double -> Int) x))


toInt :: Double -> Maybe Int
toInt x = if isInt x then Just . floor $ x else Nothing


decimalDigits :: Double -> Maybe Int
decimalDigits x = if isInt x then Just 0 else Nothing


readNumberBase :: Int -> String -> Value
readNumberBase base input =
    case parse (numberStrBase base) "" input of
        Left _ -> Nil
        Right s -> 
            if base == 10 then
                case readMaybe s of
                    Nothing -> Nil
                    Just n -> Number n
            else
                Number (numerize base s)

--return $ sign * (numerize base digits fractionalDigits)

numberStrBase :: Int -> Parser String
numberStrBase base = do
    whitespace
    sign <- choice [
        "-"  <$ char '-',
        ""   <$ char '+',
        ""   <$ return ()
        ]
    whitespace

    digits <-
        if base == 10 then
            integralAndFractional <|> justFractional
        else
            digitsBase base

    whitespace
    eof

    return (sign ++ digits)

    where
        integralAndFractional :: Parser String
        integralAndFractional = do
            int <- digitsBase 10
            optional $ char '.'
            frac <- maybe "" ('.':) <$> (optionMaybe $ digitsBase 10)
            return (int ++ frac)

        justFractional :: Parser String
        justFractional = do
            _ <- char '.'
            frac <- digitsBase 10
            return ('0':'.':frac)


whitespace :: Parser ()
whitespace = const () <$> (many . oneOf $ " \n\t")


digitsBase :: Int -> Parser String
digitsBase base = many1 $ satisfy (isDigitBase base)


isDigitBase :: Int -> Char -> Bool
isDigitBase base d = 
    if base <= 10 then
        (dist d '0') < base
    else
        (dist d '0') < 10 ||
        (dist d 'A') < (base - 10) ||
        (dist d 'a') < (base - 10)
    where
        dist :: Char -> Char -> Int
        dist c x = if (ord c - ord x) < 0 then 1000 else (ord c - ord x)


numerize :: Int -> String -> Double
numerize base digits = fst . foldr accum (0 :: Double, 0 :: Int) $ digits
    where
        accum d (v, n) = (v + numDigitPos n d, n + 1)

        numDigitPos :: Int -> Char -> Double
        numDigitPos n d = fromIntegral $ (numDigit d) * (base ^ n)

        numDigit d = fromJust . digitToIntBase base $ d


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