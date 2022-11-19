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
import           Trainable                      ( Trainable(select) )
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
    legss                              <- map ns <$> iterateR (startNetGA params >>= runNetGA params) 6
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
        bots          = map Bot (transpose legss)
        fits          = map (getBotFit $ i params) bots
        addFitToLeg (leg, fs) f = (leg, f : fs)
        newLegssWithFits = (zipWith . zipWith) addFitToLeg legssWithFits (transpose $ map (replicate 6) fits)
    swarmTestR (numTests params - 1) (i params) newLegssWithFits bots fits

swarmTestR :: Int -> Int -> [[(Net, [Float])]] -> [Bot] -> [Float] -> Rand StdGen ([[Net]], [[Float]], [Bot], [Float])
swarmTestR 0 _ legssWithFits bots fits = do
    let legss = (map . map) fst legssWithFits
        getFinalFit (_, fs) = mean fs
        fitss = (map . map) getFinalFit legssWithFits
    return (legss, fitss, bots, fits)
swarmTestR numTests iter legssWithFits bots fits = do
    orderedLegss <- mapM selectiveOrder legssWithFits
    let newBots = (map Bot . transpose . (map . map) fst) orderedLegss
        newFits = map (getBotFit iter) newBots
        addFitToLeg (leg, fs) f = (leg, f : fs)
        newLegssWithFits = (zipWith . zipWith) addFitToLeg orderedLegss (transpose $ map (replicate 6) newFits)
    swarmTestR (numTests - 1) iter newLegssWithFits (bots ++ newBots) (fits ++ newFits)

selectiveOrder :: [(Net, [Float])] -> Rand StdGen [(Net, [Float])]
selectiveOrder []           = return []
selectiveOrder legsWithFits = do
    let legs = map fst legsWithFits
        fits = map (mean . snd) legsWithFits
    (ordLegs, _) <- select (length legsWithFits) legs fits
    let getLegWithFits leg = head $ filter (\legWithFits -> fst legWithFits == leg) legsWithFits
    return $ map getLegWithFits ordLegs

getBotFit :: Int -> Bot -> Float
getBotFit iter = fst . last . getBotPath . getLegMoves iter
