module Nrn where

import           Control.DeepSeq                ( NFData )
import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                , liftRand
                                                , randomR
                                                )
import           GHC.Generics                   ( Generic )
import           Util                           ( bin2Dec
                                                , dec2Bin
                                                , dec2SignedBin
                                                , iterateR
                                                , numsInBtw
                                                , signedBin2Dec
                                                , toMaybe
                                                )

data Nrn = Nrn
    { getV   :: !Int
    , getT   :: !Int
    , getS   :: !Int
    , getWs  :: [Int]
    , getIWs :: [Int]
    }
    deriving (Eq, Show, Read, Generic, NFData)

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

nrn2Chrom :: Nrn -> Maybe [String]
nrn2Chrom (Nrn v t s ws iWs) = do
    vC   <- num2Chrom 'v' v
    tC   <- num2Chrom 't' t
    sC   <- num2Chrom 's' s
    wCs  <- mapM (num2Chrom 'w') ws
    iWCs <- mapM (num2Chrom 'w') iWs
    let chrom = vC : tC : sC : wCs ++ iWCs
    return chrom

chrom2Nrn :: Int -> [String] -> Maybe Nrn
chrom2Nrn numNrns chrom = do
    v <- chrom2Num 'v' (head chrom)
    t <- chrom2Num 't' (chrom !! 1)
    s <- chrom2Num 's' (chrom !! 2)
    let (wCs, iWCs) = splitAt numNrns (drop 3 chrom)
    ws  <- mapM (chrom2Num 'w') wCs
    iWs <- mapM (chrom2Num 'w') iWCs
    let nrn = Nrn v t s ws iWs
    return nrn

-- formatted binary string to decimal
chrom2Num :: Char -> String -> Maybe Int
chrom2Num char chrom =
    let (minVal, maxVal) = getRange char
        num              = if minVal < 0 then signedBin2Dec chrom else bin2Dec chrom
    in  toMaybe (num >= minVal && num <= maxVal) num

-- decimal to formatted binary string
num2Chrom :: Char -> Int -> Maybe String
num2Chrom char num =
    let (minVal, maxVal) = getRange char
        unpaddedBin      = if minVal < 0 then dec2SignedBin num else dec2Bin num
        paddedBin        = getPaddedBin char unpaddedBin
    in  toMaybe (num >= minVal && num <= maxVal) paddedBin

getPaddedBin :: Char -> String -> String
getPaddedBin char bin =
    let (minVal, maxVal) = getRange char
        (b : rest)       = bin
        extra0s          = replicate (getNumBits char - length bin) '0'
        padBin | minVal < 0 = b : (extra0s ++ rest)
               | otherwise  = extra0s ++ bin
    in  padBin

getNumBits :: Char -> Int
getNumBits = ceiling . logBase 2 . fromIntegral . numsInBtw . getRange
