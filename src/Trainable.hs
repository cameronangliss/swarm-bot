module Trainable where

import           Bot                            ( Bot(Bot)
                                                , getNets
                                                )
import           BotSim                         ( getBotPath )
import           Chromable                      ( Chromable(..) )
import           Control.Monad                  ( zipWithM )
import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                , liftRand
                                                , random
                                                )
import           Net                            ( Net )
import           NetSim                         ( forwardMoveLeg
                                                , testNet
                                                )
import           Util                           ( fromIntTup
                                                , remove
                                                )

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
            normedFits  = map (/ sum shiftedFits) shiftedFits
            probsList   = [ sum $ take n normedFits | n <- [1 .. length normedFits - 1] ] ++ [1.0]
            index       = minimum [ n | n <- [0 .. length probsList - 1], r <= probsList !! n ]
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

instance Trainable Bot [[[String]]] where
    select 0       _    _    = return ([], [])
    select numNets bots fits = do
        r <- liftRand random
        let shiftedFits = map (subtract (minimum fits - 1)) fits
            normedFits  = map (/ sum shiftedFits) shiftedFits
            probsList   = [ sum $ take n normedFits | n <- [1 .. length normedFits - 1] ] ++ [1.0]
            index       = minimum [ n | n <- [0 .. length probsList - 1], r <= probsList !! n ]
            selectedBot = bots !! index
            selectedFit = fits !! index
        (restBots, restFits) <- select (numNets - 1) (remove index bots) (remove index fits)
        return (selectedBot : restBots, selectedFit : restFits)
    cross bot1 bot2 = Bot <$> zipWithM cross (getNets bot1) (getNets bot2)
    mutate mutRate bot = Bot <$> mapM (mutate mutRate) (getNets bot)
    test iter = fst . last . getBotPath iter
