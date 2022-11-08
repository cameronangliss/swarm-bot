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
    legss <- iterateR (makeRandNets (numNrns params) (numNets params)) 6
    let bots    = map Bot (transpose legss)
        fits    = map (test $ i params) bots
        bestBot = getBestAgent bots fits
        bestFit = maximum fits
        maxBot  = bestBot
        maxFit  = bestFit
        avgFit  = mean fits
        fitss   = transpose $ map (replicate 6) fits
        records = BotRecords [maxBot] [maxFit] bestBot [bestFit] [avgFit] legss fitss
    return records

runBotGA :: Params -> BotRecords -> Rand StdGen BotRecords
runBotGA params records = do
    mixedLegss <- mapM shuffleM (legss records)
    let mixedFitss = (transpose . map (replicate 6 . test (i params) . Bot) . transpose) mixedLegss
    childLegss <- zipWithM (evolveNetPop params) mixedLegss mixedFitss
    let childBots  = map Bot (transpose childLegss)
        childFits  = map (test $ i params) childBots
        parentBots = (map Bot . transpose . legss) records
        parentFits = (map head . transpose . fitss) records
    (newBots, newFits) <- select (numNets params) (childBots ++ parentBots) (childFits ++ parentFits)
    let newBestBot = getBestAgent newBots newFits
        newBestFit = maximum newFits
        newMaxBot  = getBestAgent [newBestBot, last (maxBs records)] [newBestFit, last (maxFs records)]
        newMaxFit  = max newBestFit (last $ maxFs records)
        newAvgFit  = mean newFits
        newLegss   = transpose $ map getNets newBots
        newFitss   = transpose $ map (replicate 6) newFits
        newRecords = BotRecords (maxBs records ++ [ newMaxBot | newBestFit > last (maxFs records) ])
                                (maxFs records ++ [newMaxFit])
                                newBestBot
                                (bestFs records ++ [newBestFit])
                                (avgFs records ++ [newAvgFit])
                                newLegss
                                newFitss
    return newRecords
