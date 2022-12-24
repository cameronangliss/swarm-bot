module NetSim where

import           Net                            ( Net(..)
                                                , getNrns
                                                , makeNet
                                                )
import           Nrn                            ( Nrn(..)
                                                , setV
                                                )
import           Util                           ( dotProd
                                                , insert
                                                , replace
                                                )

data TestData = TestData
    { s1   :: Int
    , s2   :: Int
    , s3   :: Int
    , x    :: Int
    , y    :: Int
    , xMom :: Int
    , yMom :: Int
    , poss :: [(Int, Int)]
    }
    deriving (Show, Read)

defaultTestData :: TestData
defaultTestData = TestData 15 0 15 0 0 0 0 [(0, 0)]

-- experimentally derived horiz positions for bot
xLst :: [Int]
xLst = [0, 1, 7, 16, 28, 41, 52, 63, 71, 75, 77, 76, 73, 73, 73, 73]

-- experimentally derived vert positions for bot
yLst :: [Int]
yLst = [0, 0, 3, 8, 14, 21, 28, 34, 39, 43, 45, 48, 49, 49, 49, 49]

-- calculates the forward movement of a leg given its positional data from a simulation run
forwardMoveLeg :: [(Float, Float)] -> Float
forwardMoveLeg []                             = 0
forwardMoveLeg [_                           ] = 0
forwardMoveLeg ((x1, _) : (x2, y2) : posTail) = (x2 - x1) / (1 + y2) + forwardMoveLeg ((x2, y2) : posTail)

-- runs a simulation of an individual leg walking for the given number of iterations, returns the leg's positional data throughout the simulation
testNet :: Int -> Net -> [(Int, Int)]
testNet iter net = (poss . snd . testNetr [[15, 0, 15]] numNrns False iter 0 net) defaultTestData
    where numNrns = length (getNrns net)

testNetr :: [[Int]] -> Int -> Bool -> Int -> Int -> Net -> TestData -> (Net, TestData)
testNetr _ _ _ 0 _ net testData = (net, testData)
testNetr sLsts numNrns isFromBot iter netIndex net testData =
    let
  -- updating neuron values of selected neural network
        accums           = getNetAccs net netIndex sLsts numNrns isFromBot
        (tLst, sLst)     = (map getT $ getNrns net, map getS $ getNrns net)
        activs           = zipWith3 activationFn accums tLst sLst
        newNet           = (makeNet . zipWith setV activs . getNrns) net
        -- updating momentum and position of leg
        [inputX, inputY] = take 2 activs
        newXMom          = getNextMom inputX (x testData) (xMom testData) xLst
        newYMom          = getNextMom inputY (y testData) (yMom testData) yLst
        newX             = getNextPos inputX (x testData) newXMom xLst
        newY             = getNextPos inputY (y testData) newYMom yLst
        -- updating sensor data
        newS1            = if newX == head xLst then 15 else 0
        newS2            = if newX == last xLst then 15 else 0
        newS3            = if newY == head yLst then 15 else 0
        newSLst          = [newS1, newS2, newS3]
        newSLsts         = replace netIndex newSLst sLsts
        -- updating testData to move to next iteration of leg testing
        newTestData      = TestData newS1 newS2 newS3 newX newY newXMom newYMom (poss testData ++ [(newX, newY)])
    in  testNetr newSLsts numNrns isFromBot (iter - 1) netIndex newNet newTestData

-- calculates the new accumulation of every neuron in a neural network
getNetAccs :: Net -> Int -> [[Int]] -> Int -> Bool -> [Int]
getNetAccs net netIndex sLsts numNrns isFromBot = map (getNrnAcc net netIndex sLsts isFromBot) [0 .. numNrns - 1]

-- calculates the new accumulation of the given neuron during an iteration
getNrnAcc :: Net -> Int -> [[Int]] -> Bool -> Int -> Int
getNrnAcc net netIndex sLsts isFromBot nrnIndex =
    let vs            = (map getV . getNrns) net
        ws            = getWs (getNrns net !! nrnIndex)
        iWs           = getIWs (getNrns net !! nrnIndex)
        iWs'          = insert netIndex 0 iWs
        -- inter-neuron accumulation
        nrnAcc        = dotProd vs ws
        -- sensor accumulation for single-leg motion
        sLst          = head sLsts
        sensAcc       = dotProd sLst iWs
        legSensorAcc  = if nrnIndex `elem` [0, 1] then sensAcc else 0
        -- sensor accumulation for bot motion
        outNrnSensAcc = dotProd (sLsts !! netIndex) iWs
        hidNrnSensAcc = dotProd (map (!! 2) sLsts) iWs'
        botSensorAcc  = if nrnIndex `elem` [0, 1] then outNrnSensAcc else hidNrnSensAcc
        -- decides which sensor accumulation to use
        sensorAcc     = if isFromBot then botSensorAcc else legSensorAcc
    in  nrnAcc + sensorAcc

-- calculates the output of a neuron based on its current accumulation and its threshold values
activationFn :: Int -> Int -> Int -> Int
activationFn acc t 0 = if acc >= t then 15 else 0
activationFn acc t s = round $ 15 / (1 + exp power) where power = fromIntegral (t - acc) / fromIntegral s

getNextMom :: Int -> Int -> Int -> [Int] -> Int
getNextMom input prev mom lst =
    let req              = lst !! input
        momIsChangingDir = (mom < 0 && req - prev > 0) || (mom > 0 && req - prev < 0)
        newMom | momIsChangingDir = if mom < 0 then 1 else -1
               | req == prev      = 0
               | abs mom == 3     = mom
               | mom < 0          = mom - 1
               | mom > 0          = mom + 1
               | req > prev       = 1
               | otherwise        = -1
    in  newMom

-- calculates the new x or y position of a leg after the current iteration
getNextPos :: Int -> Int -> Int -> [Int] -> Int
getNextPos input prev mom lst =
    let desired = lst !! input
        cap     = floor $ if lst == xLst then 10 * 2 ^^ (abs mom - 3) else 5 * 2 ^^ (abs mom - 3)
        new | mom > 0   = if desired > prev then if desired - prev <= cap then desired else prev + cap else prev
            | mom < 0   = if desired < prev then if prev - desired <= cap then desired else prev - cap else prev
            | otherwise = prev
    in  new
