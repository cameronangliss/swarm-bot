module BotGA where

import           Bot                            ( Bot(..) )
import           BotSim                         ( getBotFit
                                                , getBotFitss
                                                )
import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                )
import           Net                            ( Net
                                                , makeRandNets
                                                )
import           NetGA                          ( NetParams(NetParams)
                                                , evolveNetPop
                                                , getBestAgent
                                                )
import           Util                           ( iterateR
                                                , mean
                                                , replace
                                                , split
                                                )

data BotParams = BotParams
    { numBs :: !Int
    , numNs :: !Int
    , genP  :: !Int
    , i     :: !Int
    , xT    :: !Float
    , mut   :: !Float
    , s     :: !Int
    }

instance Show BotParams where
    show pars =
        let segs =
                map show [numBs pars, numNs pars, genP pars, i pars] ++ map show [xT pars, mut pars] ++ [show $ s pars]
        in  (init . foldl (++) "Bot_" . map (++ "_")) segs

instance Read BotParams where
    readsPrec _ input =
        let inputStrs                         = tail $ split '_' input
            [numBots, numNrns, genPart, iter] = map read (take 4 inputStrs)
            [crossType, mut]                  = (map read . take 2 . drop 4) inputStrs
            seed                              = read (last inputStrs)
        in  [(BotParams numBots numNrns genPart iter crossType mut seed, "")]

data BotRecords = BotRecords
    { maxBs  :: [Bot]
    , maxFs  :: [Float]
    , bestFs :: [Float]
    , avgFs  :: [Float]
    , refBs  :: [Bot]
    , refFs  :: [Float]
    , bss    :: [[Bot]]
    , fss    :: [[Float]]
    }
    deriving (Show, Read)

data BotssData = BotssData
    { maxB   :: Bot
    , maxF   :: !Float
    , bestF  :: !Float
    , avgF   :: !Float
    , refB   :: Bot
    , refF   :: !Float
    , newBss :: [[Bot]]
    , newFss :: [[Float]]
    }

-- runs a fully automated genetic algorithm of bots over a given number of generations
-- consists of 6 parallel genetic algorithms for each leg of the bot
-- crossType is the % chance that evolution will have inter-gene crossover as opposed to intra-gene crossover, and is a float from 0 to 1
getInitBotRecords :: BotParams -> Rand StdGen BotRecords
getInitBotRecords params = do
    refNets <- makeRandNets (numNs params) 6
    let refBot = Bot refNets
        refFit = getBotFit (i params) refBot
    legss <- iterateR (makeRandNets (numNs params) (numBs params)) 6
    let botss   = applyRefToLegss refBot legss
        fitss   = getBotFitss (i params) botss
        bestBot = getBestAgent (zipWith getBestAgent botss fitss) (map maximum fitss)
        bestFit = maximum $ map maximum fitss
        maxBot  = getBestAgent [refBot, bestBot] [refFit, bestFit]
        maxFit  = max refFit bestFit
        avgFit  = mean (concat fitss)
        records = BotRecords [maxBot] [maxFit] [bestFit] [avgFit] [refBot] [refFit] botss fitss
    return records

runBotGA :: BotParams -> BotRecords -> Rand StdGen BotRecords
runBotGA params records = do
    let numGens         = length (maxFs records)
        isUpdateRefTime = numGens /= 0 && numGens `mod` genP params == 0
    newBotss <- mapM (evolveBotPop params records) [0 .. 5]
    let newFitss   = getBotFitss (i params) newBotss
        botssData  = getBotssData params records newBotss newFitss isUpdateRefTime
        newRecords = updRecords botssData records isUpdateRefTime
    if isUpdateRefTime then return newRecords else runBotGA params newRecords

-- progresses by a single generation for a given bot population
evolveBotPop :: BotParams -> BotRecords -> Int -> Rand StdGen [Bot]
evolveBotPop params records legID = do
    let refBot    = last (refBs records)
        bots      = bss records !! legID
        fits      = fss records !! legID
        eliteBot  = getBestAgent bots fits
        legs      = stripRefBots legID bots
        netParams = NetParams (numBs params) (numNs params) (i params) (xT params) (mut params) (s params)
    childNets <- evolveNetPop netParams legs fits
    let childBots = applyRefToLegs refBot legID childNets ++ [eliteBot]
    return childBots

getBotssData :: BotParams -> BotRecords -> [[Bot]] -> [[Float]] -> Bool -> BotssData
getBotssData params records botss fitss isUpdateRefTime =
    let
        newRefBot  = if isUpdateRefTime then updRefBot botss fitss else last (refBs records)
        newRefFit  = if isUpdateRefTime then getBotFit (i params) newRefBot else last (refFs records)
        newBotss   = if isUpdateRefTime then updBotss newRefBot botss else botss
        newFitss   = if isUpdateRefTime then getBotFitss (i params) newBotss else fitss
        newBestBot = getBestAgent (zipWith getBestAgent newBotss newFitss) (map maximum newFitss)
        newBestFit = maximum $ map maximum newFitss
        maxBot =
            getBestAgent [newRefBot, newBestBot, last (maxBs records)] [newRefFit, newBestFit, last (maxFs records)]
        maxFit = maximum [newRefFit, newBestFit, last (maxFs records)]
        avgFit = mean (concat newFitss)
    in
        BotssData maxBot maxFit newBestFit avgFit newRefBot newRefFit newBotss newFitss

updRefBot :: [[Bot]] -> [[Float]] -> Bot
updRefBot botss fitss = Bot $ zipWith (!!) (map getNets bestBots) [0 .. 5]
    where bestBots = zipWith getBestAgent botss fitss

updBotss :: Bot -> [[Bot]] -> [[Bot]]
updBotss refBot = applyRefToLegss refBot . stripRefBotss

updRecords :: BotssData -> BotRecords -> Bool -> BotRecords
updRecords botssData records isUpdateRefTime =
    let foundNewRecord = maxF botssData > last (maxFs records)
        newMaxBs       = maxBs records ++ [ maxB botssData | foundNewRecord ]
        newMaxFs       = maxFs records ++ [maxF botssData]
        newBestFs      = bestFs records ++ [bestF botssData]
        newAvgFs       = avgFs records ++ [avgF botssData]
        newRefBs       = refBs records ++ [ refB botssData | isUpdateRefTime ]
        newRefFs       = refFs records ++ [ refF botssData | isUpdateRefTime ]
    in  BotRecords newMaxBs newMaxFs newBestFs newAvgFs newRefBs newRefFs (newBss botssData) (newFss botssData)

applyRefToLegss :: Bot -> [[Net]] -> [[Bot]]
applyRefToLegss refBot = zipWith (applyRefToLegs refBot) [0 .. 5]

applyRefToLegs :: Bot -> Int -> [Net] -> [Bot]
applyRefToLegs refBot legID = map (applyRefToLeg refBot legID)

-- generates a bot made of the reference legs, but with one of the ref legs replaced with the inputted NN
applyRefToLeg :: Bot -> Int -> Net -> Bot
applyRefToLeg refBot legID = Bot . replace refLegs legID where refLegs = getNets refBot

stripRefBotss :: [[Bot]] -> [[Net]]
stripRefBotss = zipWith stripRefBots [0 .. 5]

stripRefBots :: Int -> [Bot] -> [Net]
stripRefBots legID = map ((!! legID) . getNets)
