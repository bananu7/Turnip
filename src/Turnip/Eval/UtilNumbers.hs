{-# LANGUAGE TupleSections #-}

module Turnip.Eval.UtilNumbers
    (isInt
    ,toInt
    ,decimalDigits
    ,readNumberBase
    )
where

import Data.Char (ord)
import Data.Maybe (fromMaybe, fromJust)
import Turnip.Eval.Types (Value(..))
import Text.ParserCombinators.Parsec

isInt :: Double -> Bool
isInt x = x == (fromIntegral ((floor :: Double -> Int) x))


toInt :: Double -> Maybe Int
toInt x = if isInt x then Just . floor $ x else Nothing


decimalDigits :: Double -> Maybe Int
decimalDigits x = if isInt x then Just 0 else Nothing


readNumberBase :: Int -> String -> Value
readNumberBase base input = case parse (numberBase base) "" input of
    Left _ -> Nil
    Right n -> Number n


numberBase :: Int -> Parser Double
numberBase base = do
    whitespace
    sign <- choice [
        (-1) <$ char '-',
        1    <$ char '+',
        1    <$ return ()
        ]
    whitespace

    (digits, fractionalDigits) <- integralAndFractional <|> justFractional

    whitespace
    eof

    return $ sign * (numerize base digits fractionalDigits)

    where
        integralAndFractional :: Parser (String, String)
        integralAndFractional = do
            int <- digitsBase base
            optional $ char '.'
            frac <- fromMaybe "" <$> (optionMaybe $ digitsBase base)
            return (int, frac)

        justFractional :: Parser (String, String)
        justFractional = do
            _ <- char '.'
            frac <- digitsBase base
            return ("", frac)


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


numerize :: Int -> String -> String -> Double
numerize base digits fractionalDigits = integralPart + fractionalPart
    where
        integralPart :: Double
        integralPart = fst . foldr accumIntegral (0 :: Double, 0 :: Int) $ digits
        accumIntegral d (v, n) = 
            (v + numDigitPos n d, n + 1)

        fractionalPart :: Double
        fractionalPart = fst . foldl accumFractional (0 :: Double, 1 :: Int) $ fractionalDigits
        accumFractional (v, n) d = 
            (v + numDigitPosInv n d, n + 1)

        numDigitPos :: Int -> Char -> Double
        numDigitPos n d = fromIntegral $ (numDigit d) * (base ^ n)

        numDigitPosInv :: Int -> Char -> Double
        numDigitPosInv n d = (fromIntegral . numDigit $ d) * (fromIntegral base ** (fromIntegral (-n)))
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