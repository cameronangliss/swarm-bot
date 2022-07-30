module Util where

import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                , liftRand
                                                , randomR
                                                )

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

replace :: [a] -> Int -> a -> [a]
replace lst n x = take n lst ++ [x] ++ drop (n + 1) lst

remove :: Int -> [a] -> [a]
remove i xs = take i xs ++ drop (i + 1) xs

split :: Char -> String -> [String]
split c = words . map (\x -> if x == c then ' ' else x)

countRecordBreaks :: Ord a => [a] -> Int
countRecordBreaks []             = 0
countRecordBreaks [x           ] = 1
countRecordBreaks (x1 : x2 : xs) = (if x1 < x2 then 1 else 0) + countRecordBreaks (x2 : xs)

bin2Dec :: String -> Int
bin2Dec ""  = 0
bin2Dec bin = read [last bin] + 2 * bin2Dec (init bin)

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

formatTime :: String -> String
formatTime timeStr =
    let timeExact = read (init timeStr) :: Float
        time      = floor timeExact
        seconds   = fromIntegral (round (10 * timeExact) `mod` 100) / 10
        minutes   = time `div` 60 `mod` 60
    in  show minutes ++ "m " ++ show seconds ++ "s"

toMaybe :: Bool -> a -> Maybe a
toMaybe True  x = Just x
toMaybe False _ = Nothing
