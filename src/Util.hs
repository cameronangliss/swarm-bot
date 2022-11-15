module Util where

import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                )
import           Data.Char                      ( digitToInt )
import           Data.List                      ( foldl' )

-- iterates through a given function, returning a list of all outcomes of the function throughout all the calls made to it
iterateR :: Rand StdGen a -> Int -> Rand StdGen [a]
iterateR _ 0 = return []
iterateR s n = do
    a    <- s
    rest <- iterateR s (n - 1)
    return (a : rest)

fromTup :: (a, a) -> [a]
fromTup (x, y) = [x, y]

fromIntTup :: (Int, Int) -> (Float, Float)
fromIntTup (x, y) = (fromIntegral x, fromIntegral y)

numsInBtw :: (Int, Int) -> Int
numsInBtw (x, y) = y - x + 1

insert :: Int -> a -> [a] -> [a]
insert i x xs = take i xs ++ [x] ++ drop i xs

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs

remove :: Int -> [a] -> [a]
remove i xs = take i xs ++ drop (i + 1) xs

split :: Char -> String -> [String]
split c = words . map (\x -> if x == c then ' ' else x)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

countRecordBreaks :: Ord a => [a] -> Int
countRecordBreaks []             = 0
countRecordBreaks [_           ] = 1
countRecordBreaks (x1 : x2 : xs) = (if x1 < x2 then 1 else 0) + countRecordBreaks (x2 : xs)

bin2Dec :: String -> Int
bin2Dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

dec2Bin :: Int -> String
dec2Bin 0   = ""
dec2Bin num = dec2Bin (num `div` 2) ++ show (num `mod` 2)

signedBin2Dec :: String -> Int
signedBin2Dec "" = 0
signedBin2Dec (c : str) | c == '1'  = negate (bin2Dec str)
                        | otherwise = bin2Dec str

dec2SignedBin :: Int -> String
dec2SignedBin num | num < 0   = '1' : dec2Bin (negate num)
                  | otherwise = '0' : dec2Bin num

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

dotProd :: Num a => [a] -> [a] -> a
dotProd as bs = foldl' (\acc (a, b) -> acc + a * b) 0 $ zip as bs
