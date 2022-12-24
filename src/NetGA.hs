module NetGA where

import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                , liftRand
                                                , randomR
                                                )
import           Data.List                      ( foldl' )
import           Net                            ( Net
                                                , makeRandNets
                                                )
import           Trainable                      ( Trainable(..) )
import           Util                           ( insert
                                                , iterateR
                                                , mean
                                                , split
                                                )

data Params = Params
    { numNets  :: Int
    , numNrns  :: Int
    , initGens :: Int
    , i        :: Int
    , mut      :: Float
    , elitism  :: Bool
    , s        :: Int
    }

instance Show Params where
    show pars =
        let segs =
                map show [numNets pars, numNrns pars, initGens pars, i pars]
                    ++ [show $ mut pars]
                    ++ [show $ elitism pars]
                    ++ [show $ s pars]
        in  init $ concatMap (++ "_") segs

instance Read Params where
    readsPrec _ input =
        let inputStrs = split '_' input
            [numNets, numNrns, initGens, iter] = map read (take 4 inputStrs)
            mut       = read (inputStrs !! 4)
            elitism   = read (inputStrs !! 5)
            seed      = read (last inputStrs)
        in  [(Params numNets numNrns initGens iter mut elitism seed, "")]

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
        maxFit     = maximum newFits
        avgFit     = mean newFits
        newRecords = NetRecords (maxNs records ++ [maxNet])
                                (maxFs records ++ [maxFit])
                                (avgFs records ++ [avgFit])
                                newNets
                                newFits
    if length (maxFs newRecords) > initGens params then return newRecords else runNetGA params newRecords

-- gets agent with best fitness in a given population
getBestAgent :: Ord b => [a] -> [b] -> a
getBestAgent agents fits = fst $ foldl' agentMax (head agents, head fits) (zip agents fits)
    where agentMax (agent1, fit1) (agent2, fit2) = if fit1 > fit2 then (agent1, fit1) else (agent2, fit2)

-- generates an entire new population of legs reproductively from the old population
evolveNetPop :: Params -> [Net] -> [Float] -> Rand StdGen [Net]
evolveNetPop params nets fits
    | elitism params = do
        let eliteNet = head [ nets !! n | n <- [0 .. length fits - 1], fits !! n == maximum fits ]
        newNets <- iterateR (getNextNetChild nets fits (mut params)) (numNets params - 1)
        r       <- liftRand $ randomR (0, numNets params - 1)
        return $ insert r eliteNet newNets
    | otherwise = iterateR (getNextNetChild nets fits (mut params)) (numNets params)

getNextNetChild :: [Net] -> [Float] -> Float -> Rand StdGen Net
getNextNetChild nets fits mut = do
    (parents, _) <- select 2 nets fits
    child        <- cross (head parents) (last parents)
    mutate mut child
