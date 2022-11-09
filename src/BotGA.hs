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
import           System.Random.Shuffle          ( shuffleM )
import           Trainable                      ( Trainable(..) )
import           Util                           ( iterateR
                                                , mean
                                                , remove
                                                , replace
                                                , split
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
    (shuffledLegss, fitss, bots, fits) <- swarmTest (numTests params) (i params) ((map . map) (, []) legss) [] []
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
    (shuffledLegss, newFitss, newBots, newFits) <- swarmTest (numTests params)
                                                             (i params)
                                                             ((map . map) (, []) childLegss)
                                                             []
                                                             []
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

swarmTest :: Int -> Int -> [[(Net, [Float])]] -> [Bot] -> [Float] -> Rand StdGen ([[Net]], [[Float]], [Bot], [Float])
swarmTest 0 _ legssWithFits bots fits = do
    let getFinalFit (leg, fits) = (leg, mean fits)
        avgFitLegss    = (map . map) getFinalFit legssWithFits
        (legss, fitss) = unzip $ map unzip avgFitLegss
    return (legss, fitss, bots, fits)
swarmTest numTests iter legssWithFits bots fits = do
    shuffledLegss <- mapM shuffleM legssWithFits
    let newBots = (map Bot . transpose . (map . map) fst) shuffledLegss
        newFits = map (test iter) newBots
        fitss   = transpose $ map (replicate 6) newFits
        addFitToLeg (leg, fs) f = (leg, f : fs)
        newLegssWithFits = (zipWith . zipWith) addFitToLeg shuffledLegss fitss
    swarmTest (numTests - 1) iter newLegssWithFits (bots ++ newBots) (fits ++ newFits)
