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
import           Util                           ( iterateR
                                                , mean
                                                , remove
                                                , replace
                                                , split
                                                )

data BotRecords = BotRecords
    { bestBs :: [Bot]
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
    let bots         = map Bot (transpose legss)
        fits         = getBotFits (i params) bots
        bestBot      = getBestAgent bots fits
        bestFit      = maximum fits
        avgFit       = mean fits
        bestBotIndex = head [ n | n <- [0 .. length fits - 1], fits !! n == bestFit ]
        ordBots      = if elitism params then bestBot : remove bestBotIndex bots else bots
        ordFits      = if elitism params then bestFit : remove bestBotIndex fits else fits
        ordLegss     = if elitism params then transpose $ map getNets ordBots else legss
        ordFitss     = transpose $ map (replicate 6) ordFits
        records      = BotRecords [bestBot] [bestFit] [avgFit] ordLegss ordFitss
    return records

runBotGA :: Params -> BotRecords -> Rand StdGen BotRecords
runBotGA params records = do
    newLegss <- zipWithM (evolveNetPop params) (legss records) (fitss records)
    let bots         = map Bot (transpose newLegss)
        fits         = getBotFits (i params) bots
        newBestBot   = getBestAgent bots fits
        newBestFit   = maximum fits
        newAvgFit    = mean fits
        bestBotIndex = head [ n | n <- [0 .. length fits - 1], fits !! n == newBestFit ]
        ordBots      = if elitism params then newBestBot : remove bestBotIndex bots else bots
        ordFits      = if elitism params then newBestFit : remove bestBotIndex fits else fits
        ordLegss     = if elitism params then transpose $ map getNets ordBots else newLegss
        ordFitss     = transpose $ map (replicate 6) ordFits
        newRecords   = BotRecords (bestBs records ++ [ newBestBot | newBestFit > last (bestFs records) ])
                                  (bestFs records ++ [newBestFit])
                                  (avgFs records ++ [newAvgFit])
                                  ordLegss
                                  ordFitss
    return newRecords
