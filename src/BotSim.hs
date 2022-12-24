module BotSim where

import           Bot                            ( Bot(..) )
import           Net                            ( Net
                                                , getNrns
                                                )
import           NetSim                         ( TestData(poss, s1, s2, s3)
                                                , defaultTestData
                                                , testNetr
                                                )
import           Util                           ( dotProd
                                                , fromIntTup
                                                )

data PathData = PathData
    { x    :: Float
    , y    :: Float
    , dir  :: Float
    , path :: [(Float, Float)]
    }
    deriving (Show, Read)

botWidth :: Float
botWidth = 240

defaultPathData :: PathData
defaultPathData = PathData 0 0 0 [(0, 0)]

-- transforms the positional data from a bot's simulated run from horiz/vert data of every leg into incremental radial data of the overall robot's position in 2D space
getBotPath :: [[(Int, Int)]] -> [(Float, Float)]
getBotPath legMoves = getBotPathr fPosLsts defaultPathData where fPosLsts = (map . map) fromIntTup legMoves

getBotPathr :: [[(Float, Float)]] -> PathData -> [(Float, Float)]
getBotPathr ([_] : _) pathData = path pathData
getBotPathr posLsts pathData =
    let verts             = [ snd $ posLst !! 1 | posLst <- posLsts ] -- list of y pos of all legs of bot at current iteration
        dragFactor        = getDragFactor verts
        vLegs2Bot = zipWith (-) [ fst $ posLst !! 1 | posLst <- posLsts ] [ fst $ head posLst | posLst <- posLsts ]
        (thrust, degTurn) = getLegEffects vLegs2Bot verts
        newDir            = dir pathData + degTurn
        newX              = x pathData + dragFactor * thrust * cos newDir
        newY              = y pathData + dragFactor * thrust * sin newDir
        newPath           = path pathData ++ [(newX, newY)]
        newPathData       = PathData newX newY newDir newPath
    in  getBotPathr (map tail posLsts) newPathData

-- interprets the list of motions being made by each leg to determine the motion of the overall bot as a result
getLegEffects :: [Float] -> [Float] -> (Float, Float)
getLegEffects vLegs2Bot verts =
    let vLegsLeft   = [ vLegs2Bot !! n | n <- [0 .. 5], even n ]
        vLegsRight  = [ vLegs2Bot !! n | n <- [0 .. 5], odd n ]
        root        = (sqrt 29 - 5) / 2
        modVerts    = map (\y -> if y < 5 then 1 / (y + root) - root else 0) verts
        leftCoeffs  = [ modVerts !! n | n <- [0 .. 5], even n ]
        rightCoeffs = [ modVerts !! n | n <- [0 .. 5], odd n ]
        denom0      = sum leftCoeffs
        denom1      = sum rightCoeffs
        coeffs0     = map (\a -> if denom0 == 0 then 0 else a / denom0) leftCoeffs
        coeffs1     = map (\a -> if denom1 == 0 then 0 else a / denom1) rightCoeffs
        thrust0     = dotProd vLegsLeft coeffs0
        thrust1     = dotProd vLegsRight coeffs1
        thrust      = (thrust0 + thrust1) / 2
        degTurn     = (thrust1 - thrust0) / botWidth
    in  (thrust, degTurn)

-- returns True if there is a tripod structure of legs all with vertical position of 0, and False otherwise
getDragFactor :: [Float] -> Float
getDragFactor ys =
    let onGround inds = and [ ys !! n == 0 | n <- inds ]
        tri          = onGround [0, 3, 4] || onGround [1, 2, 5]
        rect         = onGround [0, 1, 4, 5]
        par          = onGround [0, 2, 3, 5] || onGround [1, 2, 3, 4]
        staticStable = tri || rect || par
        numOnGround  = length [ y | y <- ys, y == 0.0 ]
        partStable   = onGround [0, 5] || onGround [2, 3] || onGround [1, 4]
        dragFactor | staticStable = 1.0
                   | partStable   = fromIntegral (numOnGround + 6) / 16
                   | otherwise    = fromIntegral numOnGround / 16
    in  dragFactor

-- simulates a bot for a given number of iterations, and returns the positional data of every leg on the bot throughout the simulation
getLegMoves :: Int -> Bot -> [[(Int, Int)]]
getLegMoves iter bot = getLegMovesr numNrns iter (getNets bot) (replicate 6 defaultTestData)
    where numNrns = (length . getNrns . head . getNets) bot

getLegMovesr :: Int -> Int -> [Net] -> [TestData] -> [[(Int, Int)]]
getLegMovesr _ 0 _ testDataLst = map poss testDataLst
getLegMovesr numNrns iter nets testDataLst =
    let s1Lst                     = map s1 testDataLst
        s2Lst                     = map s2 testDataLst
        s3Lst                     = map s3 testDataLst
        sLsts                     = [ [s1Lst !! n, s2Lst !! n, s3Lst !! n] | n <- [0 .. length testDataLst - 1] ]
        (newNets, newTestDataLst) = unzip $ zipWith3 (testNetr sLsts numNrns True 1) [0 .. 5] nets testDataLst
    in  getLegMovesr numNrns (iter - 1) newNets newTestDataLst
