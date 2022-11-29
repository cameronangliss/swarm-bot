module BotGA where

import           Bot                            ( Bot(..) )
import           BotSim                         ( getBotPath
                                                , getLegMoves
                                                )
import           Control.Monad                  ( zipWithM )
import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                )
import           Data.List                      ( transpose )
import           Net                            ( Net )
import           NetGA                          ( NetRecords(ns)
                                                , Params(..)
                                                , evolveNetPop
                                                , getBestAgent
                                                , runNetGA
                                                , startNetGA
                                                )
import           Util                           ( iterateR
                                                , mean
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
    let legParams = params { elitism = True }
    legss <- map ns <$> iterateR (startNetGA legParams >>= runNetGA legParams) 6
    let bots    = map Bot (transpose legss)
        fits    = map (getBotFit $ i params) bots
        fitss   = transpose $ map (replicate 6) fits
        bestBot = getBestAgent bots fits
        bestFit = maximum fits
        maxBot  = bestBot
        maxFit  = bestFit
        avgFit  = mean fits
        records = BotRecords [maxBot] [maxFit] bestBot [bestFit] [avgFit] legss fitss
    return records

runBotGA :: Params -> BotRecords -> Rand StdGen BotRecords
runBotGA params records = do
    childLegss <- zipWithM (evolveNetPop params) (legss records) (fitss records)
    let newBots    = map Bot (transpose childLegss)
        newFits    = map (getBotFit $ i params) newBots
        newFitss   = transpose $ map (replicate 6) newFits
        newBestBot = getBestAgent newBots newFits
        newBestFit = maximum newFits
        newMaxBot  = getBestAgent [newBestBot, last (maxBs records)] [newBestFit, last (maxFs records)]
        newMaxFit  = max newBestFit (last $ maxFs records)
        newAvgFit  = mean newFits
        newRecords = BotRecords (maxBs records ++ [ newMaxBot | newMaxFit > last (maxFs records) ])
                                (maxFs records ++ [newMaxFit])
                                newBestBot
                                (bestFs records ++ [newBestFit])
                                (avgFs records ++ [newAvgFit])
                                childLegss
                                newFitss
    return newRecords

getBotFit :: Int -> Bot -> Float
getBotFit iter = fst . last . getBotPath . getLegMoves iter
