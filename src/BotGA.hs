module BotGA where

import           Bot                            ( Bot(..) )
import           BotSim                         ( getBotFit
                                                , getBotFits
                                                )
import           Control.Monad                  ( zipWithM )
import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                , liftRand
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
import           Util                           ( iterateR
                                                , mean
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
    legss         <- iterateR (makeRandNets (numNrns params) (numNets params)) 6
    shuffledLegss <- mapM shuffleM legss
    let bots    = map Bot (transpose shuffledLegss)
        fits    = getBotFits (i params) bots
        bestBot = getBestAgent bots fits
        bestFit = maximum fits
        maxBot  = bestBot
        maxFit  = bestFit
        avgFit  = mean fits
        fitss   = transpose $ map (replicate 6) fits
        records = BotRecords [maxBot] [maxFit] bestBot [bestFit] [avgFit] shuffledLegss fitss
    return records

runBotGA :: Params -> BotRecords -> Rand StdGen BotRecords
runBotGA params records = do
    newLegss      <- zipWithM (evolveNetPop params) (legss records) (fitss records)
    shuffledLegss <- mapM shuffleM newLegss
    let bots       = map Bot (transpose shuffledLegss)
        fits       = getBotFits (i params) bots
        newBestBot = getBestAgent bots fits
        newBestFit = maximum fits
        newMaxBot  = getBestAgent [newBestBot, last (maxBs records)] [newBestFit, last (maxFs records)]
        newMaxFit  = max newBestFit (last $ maxFs records)
        newAvgFit  = mean fits
        newFitss   = transpose $ map (replicate 6) fits
        newRecords = BotRecords (maxBs records ++ [newMaxBot | newBestFit > last (maxFs records)])
                                (maxFs records ++ [newMaxFit])
                                newBestBot
                                (bestFs records ++ [newBestFit])
                                (avgFs records ++ [newAvgFit])
                                shuffledLegss
                                newFitss
    return newRecords
