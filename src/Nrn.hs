module Nrn where

import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                , liftRand
                                                , randomR
                                                )
import           Util                           ( bin2Dec
                                                , dec2Bin
                                                , dec2SignedBin
                                                , iterateR
                                                , numsInBtw
                                                , signedBin2Dec
                                                )

data Nrn = Nrn
    { getV   :: Int
    , getT   :: Int
    , getS   :: Int
    , getWs  :: [Int]
    , getIWs :: [Int]
    }
    deriving (Eq, Show, Read)

getRange :: Char -> (Int, Int)
getRange 'v' = (0, 15) -- neuron value
getRange 't' = (-511, 511) -- translation of squashing function
getRange 's' = (-255, 255) -- stretching of squashing function
getRange 'w' = (-15, 15) -- connection weight
getRange _   = (-1, 1)

setV :: Int -> Nrn -> Nrn
setV v nrn = nrn { getV = v }

-- pluralized version of below function
makeRandNrns :: Int -> Bool -> Int -> Rand StdGen [Nrn]
makeRandNrns numNrns areHidden = iterateR $ makeRandNrn numNrns areHidden

-- generates a neuron with randomly-generated fields
makeRandNrn :: Int -> Bool -> Rand StdGen Nrn
makeRandNrn numNrns isHidden = do
    v   <- (liftRand . randomR . getRange) 'v'
    t   <- (liftRand . randomR . getRange) 'v'
    s   <- (liftRand . randomR . getRange) 'v'
    ws  <- iterateR (liftRand . randomR . getRange $ 'w') numNrns
    iWs <- iterateR (liftRand . randomR . getRange $ 'w') (if isHidden then 5 else 3)
    return (Nrn v t s ws iWs)

toBin :: Char -> Int -> String
toBin char num =
    let (minVal, _) = getRange char
        unpaddedBin = if minVal < 0 then dec2SignedBin num else dec2Bin num
    in  getPaddedBin char unpaddedBin

fromBin :: Char -> String -> Int
fromBin char chrom = if minVal < 0 then signedBin2Dec chrom else bin2Dec chrom where (minVal, _) = getRange char

getPaddedBin :: Char -> String -> String
getPaddedBin char bin =
    let (minVal, _) = getRange char
        (b : rest)  = bin
        extra0s     = replicate (getNumBits char - length bin) '0'
        padBin | minVal < 0 = b : (extra0s ++ rest)
               | otherwise  = extra0s ++ bin
    in  padBin

getNumBits :: Char -> Int
getNumBits = ceiling . logBase 2 . fromIntegral . numsInBtw . getRange
