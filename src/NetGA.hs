module NetGA where

import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                )
import           Net                            ( Net
                                                , makeRandNets
                                                )
import           Trainable                      ( Trainable(..) )
import           Util                           ( iterateR
                                                , mean
                                                , split
                                                )

data Params = Params
    { numNets  :: Int
    , numNrns  :: Int
    , numTests :: Int
    , i        :: Int
    , mut      :: Float
    , elitism  :: Bool
    , s        :: Int
    }

instance Show Params where
    show pars =
        let segs =
                map show [numNets pars, numNrns pars, numTests pars, i pars]
                    ++ [show $ mut pars]
                    ++ [show $ elitism pars]
                    ++ [show $ s pars]
        in  init $ concatMap (++ "_") segs

instance Read Params where
    readsPrec _ input =
        let inputStrs = split '_' input
            [numNets, numNrns, numTests, iter] = map read (take 4 inputStrs)
            mut       = read (inputStrs !! 4)
            elitism   = read (inputStrs !! 5)
            seed      = read (last inputStrs)
        in  [(Params numNets numNrns numTests iter mut elitism seed, "")]

data NetRecords = NetRecords
    { maxNs :: [Net]
    , maxFs :: [Float]
    , avgFs :: [Float]
    , ns    :: [Net]
    , fs    :: [Float]
    }
    deriving (Show, Read)

-- executes a fully-automated genetic algorithm for a given number of generations
startNetGA :: Params -> Rand StdGen NetRecords
startNetGA params = do
    nets <- makeRandNets (numNrns params) (numNets params)
    let fits    = map (test $ i params) nets
        maxNet  = getBestAgent nets fits
        maxFit  = maximum fits
        avgFit  = mean fits
        records = NetRecords [maxNet] [maxFit] [avgFit] nets fits
    return records

runNetGA :: Params -> NetRecords -> Rand StdGen NetRecords
runNetGA params records = do
    newNets <- evolveNetPop params (ns records) (fs records)
    let newFits    = map (test $ i params) newNets
        maxNet     = getBestAgent newNets newFits
        updMaxNets = maxNs records ++ [maxNet]
        maxFit     = maximum newFits
        updMaxFits = maxFs records ++ [maxFit]
        avgFit     = mean newFits
        updAvgFits = avgFs records ++ [avgFit]
        newRecords = NetRecords updMaxNets updMaxFits updAvgFits newNets newFits
    if length updMaxFits `mod` 100 == 1 then return newRecords else runNetGA params newRecords

-- gets agent with best fitness in a given population
getBestAgent :: Ord b => [a] -> [b] -> a
getBestAgent agents fits = fst $ foldl agentMax (head agents, head fits) (zip agents fits)
    where agentMax (agent1, fit1) (agent2, fit2) = if fit1 > fit2 then (agent1, fit1) else (agent2, fit2)

-- generates an entire new population of legs reproductively from the old population
evolveNetPop :: Params -> [Net] -> [Float] -> Rand StdGen [Net]
evolveNetPop params nets fits
    | elitism params = do
        let eliteNet = head [ nets !! n | n <- [0 .. length fits - 1], fits !! n == maximum fits ]
        newNets <- iterateR (getNextNetChild nets fits (mut params)) (numNets params - 1)
        return (eliteNet : newNets)
    | otherwise = iterateR (getNextNetChild nets fits (mut params)) (numNets params)

getNextNetChild :: [Net] -> [Float] -> Float -> Rand StdGen Net
getNextNetChild nets fits mut = do
    (parents, _) <- select 2 nets fits
    child        <- cross (head parents) (last parents)
    mutate mut child
