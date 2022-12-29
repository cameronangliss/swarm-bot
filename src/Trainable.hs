module Trainable where

import           Chromable            (Chromable (..))
import           Control.Monad        (zipWithM)
import           Control.Monad.Random (Rand, StdGen, liftRand, random)
import           Data.List            (scanl')
import           Net                  (Net)
import           NetSim               (forwardMoveLeg, testNet)
import           Util                 (fromIntTup, remove)

class Chromable a c => Trainable a c where
    select :: Int -> [a] -> [Float] -> Rand StdGen ([a], [Float])
    cross :: a -> a -> Rand StdGen a
    mutate :: Float -> a -> Rand StdGen a
    test :: Int -> a -> Float

instance Trainable Net [[String]] where
    select 0       _    _    = return ([], [])
    select numNets nets fits = do
        r <- liftRand random
        let shiftedFits = map (subtract (minimum fits - 1)) fits
            totalFits   = sum shiftedFits
            normedFits  = map (/ totalFits) shiftedFits
            probsList   = tail $ scanl' (+) 0 (init normedFits)
            index       = length $ takeWhile (< r) probsList
            selectedLeg = nets !! index
            selectedFit = fits !! index
        (restLegs, restFits) <- select (numNets - 1) (remove index nets) (remove index fits)
        return (selectedLeg : restLegs, selectedFit : restFits)
    cross net1 net2 = do
        let chrom1 = toChrom net1
            chrom2 = toChrom net2
        crossedChrom <- (zipWithM . zipWithM) (crossGenes 0.5) chrom1 chrom2
        return (fromChrom crossedChrom)
      where
        crossGenes :: Float -> String -> String -> Rand StdGen String
        crossGenes odds g1 g2 = do
            r <- liftRand random
            if r <= odds then return g1 else return g2
    mutate mutRate net = do
        let chrom = toChrom net
        mutChrom <- (mapM . mapM . mapM) (mutateBit mutRate) chrom
        return (fromChrom mutChrom)
      where
        mutateBit :: Float -> Char -> Rand StdGen Char
        mutateBit mutRate bit = do
            r <- liftRand random
            let flippedBit = if bit == '0' then '1' else '0'
            if r < mutRate then return flippedBit else return bit
    test iter = forwardMoveLeg . map fromIntTup . testNet iter
