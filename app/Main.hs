module Main where

import           Bot                            ( Bot(..) )
import           BotGA                          ( BotParams(BotParams, genP, i, s)
                                                , BotRecords(..)
                                                , getInitBotRecords
                                                , runBotGA
                                                )
import           BotSim                         ( getBotFitWhere
                                                , getBotPath
                                                , testBot
                                                )
import           Control.Monad.Random           ( StdGen
                                                , mkStdGen
                                                , runRand
                                                )
import           Data.Time                      ( diffUTCTime
                                                , getCurrentTime
                                                )
import           NetGA                          ( NetParams(NetParams, i, s)
                                                , NetRecords(..)
                                                , runNetGA
                                                , startNetGA
                                                )
import           NetSim                         ( testNet )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , readFile
                                                , stdout
                                                , writeFile
                                                )
import           System.Process                 ( callCommand )
import           System.Random.Internal         ( StdGen(..) )
import           System.Random.SplitMix         ( SMGen )
import           Util                           ( countRecordBreaks
                                                , formatTime
                                                , fromTup
                                                )

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nWhat would you like to do?"
    putStrLn "Options:"
    putStrLn "    newBot  -> start a new bot run based on user inputs"
    putStrLn "    loadBot -> load and continue an old saved bot run"
    putStrLn "    newLeg  -> start a new leg run based on user inputs"
    putStrLn "    loadLeg -> load and continue an old saved leg run"
    putStrLn "    q       -> exit the program"
    input <- getLine
    let action
            | input == "newBot" = newBotMain
            | input == "loadBot" = loadBotMain
            | input == "newLeg" = newLegMain
            | input == "loadLeg" = loadLegMain
            | input == "q" = putStrLn "\nGoodbye!\n"
            | otherwise = do
                putStrLn "\nInvalid input. Try again."
                main
    action

---------------------------------------- BOT MAIN ----------------------------------------

newBotMain :: IO ()
newBotMain = do
    params <- getBotParams Nothing
    putStr "\nInitializing generation 0..."
    let (records, g) = runRand (getInitBotRecords params) (mkStdGen $ BotGA.s params)
        !maxFit      = last (BotGA.maxFs records)
    putStrLn "Success!"
    viewBot params records g

loadBotMain :: IO ()
loadBotMain = do
    params <- getBotParams Nothing
    putStr "\nLoading save data..."
    recordStr <- readFile (".stack-work/runs/" ++ show params ++ ".txt")
    let (records, smgen) = read recordStr :: (BotRecords, SMGen)
        g                = StdGen smgen
        !maxFit          = last (BotGA.maxFs records)
    putStrLn "Success!"
    viewBot params records g

viewBot :: BotParams -> BotRecords -> StdGen -> IO ()
viewBot params records g = do
    putStrLn
        $  "\nCompleted "
        ++ (show . subtract 1 . length . BotGA.maxFs) records
        ++ " generations. What would you like to do?"
    putStrLn "Options:"
    putStrLn "    maxBot     -> print bot with greatest fitness from run"
    putStrLn "    maxFit     -> print greatest fitness from run"
    putStrLn "    refBot     -> print specified reference bot from run"
    putStrLn "    refFits    -> print list of reference fitnesses from run"
    putStrLn "    avgFits    -> print average fitness over all legs from each generation"
    putStrLn "    plotRaw    -> plots unaltered data from run up to desired generation"
    putStrLn "    plotSmooth -> plots smoothed data from run up to desired generation"
    putStrLn "    plotRef    -> plots movement of refBot from desired generation"
    putStrLn "    plotMax    -> plots movement of maxBot from desired generation"
    putStrLn "    power      -> tests maxBot when functionality is decreased in chosen legs"
    putStrLn "    run        -> resume run with unchanged parameters up to desired generation"
    putStrLn "    change     -> change parameters of run"
    putStrLn "    back       -> return to main menu"
    input <- getLine
    let action
            | input == "maxBot" = do
                putStrLn ""
                (print . last . BotGA.maxBs) records
                viewBot params records g
            | input == "maxFit" = do
                putStrLn ""
                (print . round . last . BotGA.maxFs) records
                viewBot params records g
            | input == "refBot" = do
                putStr "\nProvide index of desired reference bot: "
                refInd <- readLn
                putStrLn ""
                let isValid = refInd `elem` [0 .. length (refBs records)]
                if isValid then print $ refBs records !! refInd else putStrLn "Index out of bounds."
                viewBot params records g
            | input == "refFits" = do
                putStrLn ""
                (print . map round . refFs) records
                viewBot params records g
            | input == "avgFits" = do
                putStrLn ""
                (print . map round . BotGA.avgFs) records
                viewBot params records g
            | input == "plotRaw" = do
                recordBotRun params records g "raw"
                viewBot params records g
            | input == "plotSmooth" = do
                recordBotRun params records g "smooth"
                viewBot params records g
            | input == "plotRef" = do
                recordBotRun params records g "ref"
                viewBot params records g
            | input == "plotMax" = do
                recordBotRun params records g "max"
                viewBot params records g
            | input == "power" = do
                recordBotRun params records g "power"
                viewBot params records g
            | input == "run" = do
                putStr "\nEnter desired generation to compute up to: "
                input <- readLn
                if input < length (bestFs records) + genP params - 1
                    then viewBot params records g
                    else do
                        (newRecords, g2) <- computeBotRun params records g input
                        viewBot params newRecords g2
            | input == "change" = do
                newParams <- (getBotParams . Just . BotGA.s) params
                viewBot newParams records g
            | input == "back" = main
            | otherwise = do
                putStrLn "\nInvalid input. Try again."
                viewBot params records g
    action

computeBotRun :: BotParams -> BotRecords -> StdGen -> Int -> IO (BotRecords, StdGen)
computeBotRun params records g genCap = do
    putStr $ "\nComputing up to generation " ++ show (length (bestFs records) + genP params - 1) ++ "..."
    start <- getCurrentTime
    let (newRecords, g2) = runRand (runBotGA params records) g
        !maxFit          = last (BotGA.maxFs newRecords)
    end <- getCurrentTime
    putStrLn "Finished!"
    putStr $ "Duration = " ++ formatTime (show $ diffUTCTime end start)
    recordBotRun params newRecords g2 "default"
    if genCap < length (bestFs newRecords) + genP params - 1
        then return (newRecords, g2)
        else computeBotRun params newRecords g2 genCap

recordBotRun :: BotParams -> BotRecords -> StdGen -> String -> IO ()
recordBotRun params records g recordType = do
    numGens <- if recordType == "default" || recordType == "power"
        then (return . length . bestFs) records
        else if recordType == "raw" || recordType == "smooth"
            then putStr "\nEnter the generation you would like to plot up to: " >> readLn >>= (\x -> return (x + 1))
            else putStr "\nEnter desired generation of bot: " >> readLn >>= (\x -> return (x + 1))
    let truncRecords = getTruncRecords params records numGens
        bot          = if recordType == "max" || recordType == "power"
            then last (maxBs truncRecords)
            else last (refBs truncRecords)
    powerLst <- if recordType == "power"
        then do
            putStr "\nEnter list of levels of capability of bot legs (ex: [1, 1, 0.5, 0, 0.25, 1]: "
            readLn
        else return $ replicate 6 1.0
    let fit = getBotFitWhere (BotGA.i params) powerLst bot
    writeDataFile params truncRecords bot powerLst
    callCommand $ unwords
        ["python3 src/PlotBotData.py", show params, recordType, show (numGens - 1), show powerLst, show (round fit)]
    if recordType == "default"
        then do
            writeFile (".stack-work/runs/" ++ show params ++ ".txt") (show (records, unStdGen g))
            putStrLn "\nPlots drawn, save file overwritten."
        else do
            putStrLn "\nPlots drawn."

getTruncRecords :: BotParams -> BotRecords -> Int -> BotRecords
getTruncRecords params records numGens =
    let numRefs         = pred numGens `div` genP params + 1
        numRecordBreaks = (countRecordBreaks . take numGens . BotGA.maxFs) records
        newMaxBs        = take numRecordBreaks (maxBs records)
        newMaxFs        = take numGens (BotGA.maxFs records)
        newBestFs       = take numGens (bestFs records)
        newAvgFs        = take numGens (BotGA.avgFs records)
        newRefBs        = take numRefs (refBs records)
        newRefFs        = take numRefs (refFs records)
    in  BotRecords newMaxBs newMaxFs newBestFs newAvgFs newRefBs newRefFs (bss records) (fss records)

writeDataFile :: BotParams -> BotRecords -> Bot -> [Float] -> IO ()
writeDataFile params records bot powerLst = do
    let legMoves         = testBot (BotGA.i params) powerLst bot
        unzippedLegMoves = map (fromTup . unzip) legMoves
        botPath          = getBotPath (BotGA.i params) powerLst bot
        unzippedBotPath  = fromTup (unzip botPath)
        refFits          = (concatMap (replicate $ genP params) . init . refFs) records ++ [last $ refFs records]
        fitsTxt          = (unlines . map show) [BotGA.maxFs records, bestFs records, BotGA.avgFs records, refFits]
    writeFile ".stack-work/datafile.txt" $ fitsTxt ++ show unzippedLegMoves ++ "\n" ++ show unzippedBotPath

getBotParams :: Maybe Int -> IO BotParams
getBotParams currSeed = do
    let getIntLine str = do
            putStr str
            input <- getLine
            readIO input :: IO Int
        getFloatLine str = do
            putStr str
            input <- getLine
            readIO input :: IO Float
    putStrLn "\nEnter Genetic Algorithm Parameters:"
    numBots <- getIntLine "Population Size = "
    numNrns <- getIntLine "Number of Neurons Per Neural Net = "
    genPart <- getIntLine "Number of Generations Per Reference Bot = "
    iter    <- getIntLine "Number of Iterations Per Bot Test = "
    mut     <- getFloatLine "Mutation Rate = "
    seed    <- maybe (getIntLine "Initial RNG Seed = ") return currSeed
    let params = BotParams numBots numNrns genPart iter mut seed
    putStr "Are these values all correct? (y/n): "
    input <- getLine
    let action
            | input == "y" = return params
            | input == "n" = getBotParams currSeed
            | otherwise = do
                putStr "\nInvalid input. Try again."
                getBotParams currSeed
    action

---------------------------------------- LEG MAIN ----------------------------------------

newLegMain :: IO ()
newLegMain = do
    params <- getLegParams Nothing
    putStr "\nInitializing generation 0..."
    let (records, g) = runRand (startNetGA params) (mkStdGen $ NetGA.s params)
        !maxFit      = last (NetGA.maxFs records)
    putStrLn "Success!"
    viewLeg params records g

loadLegMain :: IO ()
loadLegMain = do
    params <- getLegParams Nothing
    putStr "\nLoading save data..."
    recordStr <- readFile (".stack-work\\runs\\" ++ show params ++ ".txt")
    let (records, smgen) = read recordStr :: (NetRecords, SMGen)
        g                = StdGen smgen
        !maxFit          = last (maxNs records)
    putStrLn "Success!"
    viewLeg params records g

viewLeg :: NetParams -> NetRecords -> StdGen -> IO ()
viewLeg params records g = do
    putStrLn
        $  "\nCompleted "
        ++ (show . subtract 1 . length . NetGA.maxFs) records
        ++ " generations. What would you like to do?"
    putStrLn "Options:"
    putStrLn "    maxFs   -> print max fitness from each generation"
    putStrLn "    avgFs   -> print average fitness from each generation"
    putStrLn "    record  -> draws plots of run, saves current state of run"
    putStrLn "    recordF -> plots finalized run's data, saves run"
    putStrLn "    run     -> resume the run from the current state of evolution"
    putStrLn "    back    -> return to main menu"
    input <- getLine
    let action
            | input == "maxFs" = do
                (print . map round . NetGA.maxFs) records
                viewLeg params records g
            | input == "avgFs" = do
                (print . map round . NetGA.avgFs) records
                viewLeg params records g
            | input == "record" = do
                recordLegRun params records g False
                viewLeg params records g
            | input == "recordF" = do
                recordLegRun params records g True
                viewLeg params records g
            | input == "run" = loopLeg params records g
            | input == "back" = main
            | otherwise = do
                putStrLn "\nInvalid input. Try again."
                viewLeg params records g
    action

loopLeg :: NetParams -> NetRecords -> StdGen -> IO ()
loopLeg params records g = do
    putStr $ "\nComputing generation " ++ (show . length . NetGA.maxFs) records ++ "..."
    start <- getCurrentTime
    let (newRecords, g2) = runRand (runNetGA params records) g
        !maxFit          = last (NetGA.maxFs newRecords)
    end <- getCurrentTime
    putStrLn "Success!"
    putStrLn $ "    Duration = " ++ formatTime (show $ diffUTCTime end start)
    recordLegRun params newRecords g2 False
    loopLeg params newRecords g2

recordLegRun :: NetParams -> NetRecords -> StdGen -> Bool -> IO ()
recordLegRun params records g isFinal = do
    let label        = show params
        legPosits    = testNet (NetGA.i params) $ last (maxNs records)
        (xLst, yLst) = unzip legPosits
    writeFile ".stack-work/datafile.txt"
        $  show (NetGA.maxFs records)
        ++ "\n"
        ++ show (NetGA.avgFs records)
        ++ "\n"
        ++ show xLst
        ++ "\n"
        ++ show yLst
    callCommand $ "python3 src/PlotLegData.py " ++ label ++ " " ++ show isFinal
    putStrLn "Plot successfully made!"
    writeFile (".stack-work/runs/" ++ label ++ ".txt") (show (records, unStdGen g))
    putStrLn "\nPlots drawn, run saved."

getLegParams :: Maybe Int -> IO NetParams
getLegParams currSeed = do
    let getIntLine str = do
            putStr str
            input <- getLine
            readIO input :: IO Int
        getFloatLine str = do
            putStr str
            input <- getLine
            readIO input :: IO Float
    putStrLn "\nEnter parameters:"
    numNets <- getIntLine "numNets = "
    numNrns <- getIntLine "numNrns = "
    iter    <- getIntLine "iter = "
    mut     <- getFloatLine "mut = "
    seed    <- maybe (getIntLine "Initial RNG seed = ") return currSeed
    let params = NetParams numNets numNrns iter mut seed
    putStr "Are these values all correct? (y/n): "
    input <- getLine
    let action
            | input == "y" = return params
            | input == "n" = getLegParams currSeed
            | otherwise = do
                putStr "\nInvalid input. Try again."
                getLegParams currSeed
    action
