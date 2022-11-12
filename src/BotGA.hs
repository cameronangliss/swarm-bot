module BotGA where

import           Bot                            ( Bot(..) )
import           Control.Monad                  ( zipWithM )
import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                , liftRand
                                                , random
                                                )
import           Data.List                      ( transpose )
import           Net                            ( Net
                                                , makeRandNets
                                                )
import           NetGA                          ( Params(..)
                                                , evolveNetPop
                                                , getBestAgent
                                                )
import           Trainable                      ( Trainable(..) )
import           Util                           ( iterateR
                                                , mean
                                                , remove
                                                )

data BotRecords = BotRecords
    { maxBs  :: [Bot]
    , maxFs  :: [Float]
    , bestB  :: Bot
    , bestFs :: [Float]
    , avgFs  :: [Float]
    , legss  :: [[Net]]
    , fitss  :: [[Float]]
    }
    deriving (Show, Read)

-- runs a fully automated genetic algorithm of bots over a given number of generations
-- consists of 6 parallel genetic algorithms for each leg of the bot
getInitBotRecords :: Params -> Rand StdGen BotRecords
getInitBotRecords params = do
    legss                              <- iterateR (makeRandNets (numNrns params) (numNets params)) 6
    (shuffledLegss, fitss, bots, fits) <- swarmTest params legss
    let bestBot = getBestAgent bots fits
        bestFit = maximum fits
        maxBot  = bestBot
        maxFit  = bestFit
        avgFit  = mean fits
        records = BotRecords [maxBot] [maxFit] bestBot [bestFit] [avgFit] shuffledLegss fitss
    return records

runBotGA :: Params -> BotRecords -> Rand StdGen BotRecords
runBotGA params records = do
    childLegss <- zipWithM (evolveNetPop params) (legss records) (fitss records)
    (shuffledLegss, newFitss, newBots, newFits) <- swarmTest params childLegss
    let newBestBot = getBestAgent newBots newFits
        newBestFit = maximum newFits
        newMaxBot  = getBestAgent [newBestBot, last (maxBs records)] [newBestFit, last (maxFs records)]
        newMaxFit  = max newBestFit (last $ maxFs records)
        newAvgFit  = mean newFits
        newRecords = BotRecords (maxBs records ++ [ newMaxBot | newMaxFit > last (maxFs records) ])
                                (maxFs records ++ [newMaxFit])
                                newBestBot
                                (bestFs records ++ [newBestFit])
                                (avgFs records ++ [newAvgFit])
                                shuffledLegss
                                newFitss
    return newRecords

swarmTest :: Params -> [[Net]] -> Rand StdGen ([[Net]], [[Float]], [Bot], [Float])
swarmTest params legss = do
    let legssWithFits = (map . map) (, []) legss
        newBots       = (map Bot . transpose . (map . map) fst) legssWithFits
        newFits       = map (test $ i params) newBots
        fitss         = transpose $ map (replicate 6) newFits
        addFitToLeg (leg, fs) f = (leg, f : fs)
        newLegssWithFits = (zipWith . zipWith) addFitToLeg legssWithFits fitss
    swarmTestR (numTests params - 1) (i params) newLegssWithFits newBots newFits

swarmTestR :: Int -> Int -> [[(Net, [Float])]] -> [Bot] -> [Float] -> Rand StdGen ([[Net]], [[Float]], [Bot], [Float])
swarmTestR 0 _ legssWithFits bots fits = do
    let legss = (map . map) fst legssWithFits
        getMaxFit (_, fs) = maximum fs
        fitss = (map . map) getMaxFit legssWithFits
    return (legss, fitss, bots, fits)
swarmTestR numTests iter legssWithFits bots fits = do
    orderedLegss <- mapM selectiveOrder legssWithFits
    let newBots = (map Bot . transpose . (map . map) fst) orderedLegss
        newFits = map (test iter) newBots
        fitss   = transpose $ map (replicate 6) newFits
        addFitToLeg (leg, fs) f = (leg, f : fs)
        newLegssWithFits = (zipWith . zipWith) addFitToLeg orderedLegss fitss
    swarmTestR (numTests - 1) iter newLegssWithFits (bots ++ newBots) (fits ++ newFits)

selectiveOrder :: [(Net, [Float])] -> Rand StdGen [(Net, [Float])]
selectiveOrder []           = return []
selectiveOrder legsWithFits = do
    r <- liftRand random
    let fits        = map (maximum . snd) legsWithFits
        shiftedFits = map (subtract (minimum fits - 1)) fits
        normedFits  = map (/ sum shiftedFits) shiftedFits
        probsList   = [ sum $ take n normedFits | n <- [1 .. length normedFits - 1] ] ++ [1.0]
        index       = minimum [ n | n <- [0 .. length probsList - 1], r <= probsList !! n ]
        selectedLeg = legsWithFits !! index
    rest <- selectiveOrder (remove index legsWithFits)
    return (selectedLeg : rest)
