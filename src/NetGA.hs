module NetGA where

import           Control.Monad                  ( zipWithM )
import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                , liftRand
                                                , random
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Net                            ( Net
                                                , chrom2Net
                                                , makeRandNets
                                                , net2Chrom
                                                )
import           NetSim                         ( getNetFit )
import           Util                           ( iterateR
                                                , mean
                                                , remove
                                                , split
                                                )

data NetParams = NetParams
    { numNets :: !Int
    , numNrns :: !Int
    , i       :: !Int
    , xT      :: !Float
    , mut     :: !Float
    , s       :: !Int
    }

instance Show NetParams where
    show pars =
        let segs = map show [numNets pars, numNrns pars, i pars] ++ map show [xT pars, mut pars] ++ [show $ s pars]
        in  (init . foldl (++) "Leg_" . map (++ "_")) segs

instance Read NetParams where
    readsPrec _ input =
        let inputStrs                = tail $ split '_' input
            [numNets, numNrns, iter] = map read (take 3 inputStrs)
            [crossType, mut]         = (map read . take 2 . drop 3) inputStrs
            seed                     = read (last inputStrs)
        in  [(NetParams numNets numNrns iter crossType mut seed, "")]

data NetRecords = NetRecords
    { maxNs :: [Net]
    , maxFs :: [Float]
    , avgFs :: [Float]
    , ns    :: [Net]
    , fs    :: [Float]
    }
    deriving (Show, Read)

-- executes a fully-automated genetic algorithm for a given number of generations
startNetGA :: NetParams -> Rand StdGen NetRecords
startNetGA params = do
    nets <- makeRandNets (numNrns params) (numNets params)
    let fits    = map (getNetFit $ i params) nets
        maxNet  = getBestAgent nets fits
        maxFit  = maximum fits
        avgFit  = mean fits
        records = NetRecords [maxNet] [maxFit] [avgFit] nets fits
    return records

runNetGA :: NetParams -> NetRecords -> Rand StdGen NetRecords
runNetGA params records = do
    let eliteNet = last (maxNs records)
    childNets <- evolveNetPop params (ns records) (fs records)
    let newNets    = eliteNet : childNets
        newFits    = map (getNetFit $ i params) newNets
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
evolveNetPop :: NetParams -> [Net] -> [Float] -> Rand StdGen [Net]
evolveNetPop params nets fits = iterateR (getNextNetChild nets fits (mut params) (xT params)) (numNets params - 1)

getNextNetChild :: [Net] -> [Float] -> Float -> Float -> Rand StdGen Net
getNextNetChild nets fits mut crossType = do
    parents <- selectNetParents nets fits
    genNetChild parents mut crossType

-- makes a new neural network offspring from two parents chosen probabilistically based on fitnesses of leg population
-- reproduction includes segmented crossover of chromosomes and mutation
-- interOrIntra == 0 -> inter-gene crossover (segmented crossover); interOrIntra == 1 -> intra-gene crossover (universal crossover)
genNetChild :: (Net, Net) -> Float -> Float -> Rand StdGen Net
genNetChild (p1, p2) mut crossType = do
    let p1Chrom = fromMaybe [] (net2Chrom p1)
        p2Chrom = fromMaybe [] (net2Chrom p2)
    r             <- liftRand random
    childChrom    <- if r < crossType then interCross p1Chrom p2Chrom else intraCross p1Chrom p2Chrom
    mutChildChrom <- mutNet mut childChrom
    let chrom = fromMaybe p1 (chrom2Net mutChildChrom)
    return chrom

-- chooses a pair of parent neural networks to reproduce
selectNetParents :: [Net] -> [Float] -> Rand StdGen (Net, Net)
selectNetParents nets fits = do
    rands <- iterateR (liftRand random) 2
    let shiftedFits = map (subtract (minimum fits - 1)) fits
        probsLst    = getProbsLst $ map (/ sum shiftedFits) shiftedFits
        i1          = findParentIndex (head rands) probsLst
        newProbsLst = remove i1 probsLst
        i2          = findParentIndex (last rands) newProbsLst
        i2Act       = if i1 <= i2 then i2 + 1 else i2
    return (nets !! i1, nets !! i2Act)

-- produces a list of numbers between 0 and 1
-- each neural network owns a small strip of the interval [0, 1], and the longer its strip, the more likely a random number between 0 and 1 will land in its strip
getProbsLst :: [Float] -> [Float]
getProbsLst normedFits = [ sum $ take n normedFits | n <- [1 .. length normedFits] ]

-- picks a neural network to reproduce based on its fitness (higher fitness = higher likelihood of being chosen)
findParentIndex :: Float -> [Float] -> Int
findParentIndex r probs = sum $ map (\p -> if r <= p then 0 else 1) (init probs)

-- executes crossover between two neural networks' chromosomes
interCross :: [[String]] -> [[String]] -> Rand StdGen [[String]]
interCross = (zipWithM . zipWithM) chooseObj

intraCross :: [[String]] -> [[String]] -> Rand StdGen [[String]]
intraCross = (zipWithM . zipWithM . zipWithM) chooseObj

chooseObj :: a -> a -> Rand StdGen a
chooseObj a1 a2 = do
    r <- liftRand random :: Rand StdGen Float
    if r < 0.5 then return a1 else return a2

-- mutates the chromosome of the given neural network
mutNet :: Float -> [[String]] -> Rand StdGen [[String]]
mutNet mut = mapM . mapM . mapM $ mutBit mut

mutBit :: Float -> Char -> Rand StdGen Char
mutBit mut bit = do
    r <- liftRand random
    if r < mut then return (flipBit bit) else return bit

flipBit :: Char -> Char
flipBit c = if c == '0' then '1' else '0'
