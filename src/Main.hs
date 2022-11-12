module Main where

import           Bot                            ( Bot(..) )
import           BotGA                          ( BotRecords(..)
                                                , getInitBotRecords
                                                , runBotGA
                                                )
import           BotSim                         ( getBotPath
                                                , testBot
                                                )
import           Control.Monad                  ( when )
import           Control.Monad.Random           ( StdGen
                                                , mkStdGen
                                                , runRand
                                                )
import           NetGA                          ( NetRecords(..)
                                                , Params(..)
                                                , runNetGA
                                                , startNetGA
                                                )
import           NetSim                         ( testNet )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           System.Process                 ( callCommand )
import           System.Random.Internal         ( StdGen(..) )
import           System.Random.SplitMix         ( SMGen )
import           Util                           ( countRecordBreaks
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
    params <- getParams Nothing
    putStr "\nInitializing generation 0..."
    let (records, g) = runRand (getInitBotRecords params) (mkStdGen $ s params)
        !_           = last (BotGA.maxFs records)
    putStrLn "Success!"
    viewBot params records g

loadBotMain :: IO ()
loadBotMain = do
    params <- getParams Nothing
    putStr "\nLoading save data..."
    recordStr <- readFile (".stack-work\\runs\\Bot_" ++ show params ++ ".txt")
    let (records, smgen) = read recordStr :: (BotRecords, SMGen)
        !_               = last (BotGA.maxFs records)
        g                = StdGen smgen
    putStrLn "Success!"
    viewBot params records g

viewBot :: Params -> BotRecords -> StdGen -> IO ()
viewBot params records g = do
    putStrLn
        $  "\nCompleted "
        ++ (show . subtract 1 . length . bestFs) records
        ++ " generations. What would you like to do?"
    putStrLn "Options:"
    putStrLn "    plotEvo -> plots unaltered data from run up to desired generation"
    putStrLn "    plotMax -> plots movement of bestBot from desired generation"
    putStrLn "    run     -> resume run with unchanged parameters up to desired generation"
    putStrLn "    change  -> change parameters of run"
    putStrLn "    back    -> return to main menu"
    input <- getLine
    let action
            | input == "plotEvo" = do
                recordBotRun params records g "evo"
                viewBot params records g
            | input == "plotMax" = do
                recordBotRun params records g "max"
                viewBot params records g
            | input == "run" = do
                putStr "\nEnter desired generation to compute up to: "
                input <- readLn
                if input < length (bestFs records) - 1
                    then viewBot params records g
                    else do
                        putStrLn ""
                        (newRecords, g2) <- computeBotRun params records g input
                        viewBot params newRecords g2
            | input == "change" = do
                newParams <- (getParams . Just . s) params
                viewBot newParams records g
            | input == "back" = main
            | otherwise = do
                putStrLn "\nInvalid input. Try again."
                viewBot params records g
    action

computeBotRun :: Params -> BotRecords -> StdGen -> Int -> IO (BotRecords, StdGen)
computeBotRun params records g genCap = do
    putStr $ "Computing up to generation " ++ (show . length . bestFs) records ++ "..."
    let (newRecords, g2) = runRand (runBotGA params records) g
        !_               = last (BotGA.maxFs newRecords)
    putStrLn "Finished!"
    when (length (bestFs newRecords) `mod` 100 == 1) $ recordBotRun params newRecords g2 "default"
    if genCap < length (bestFs newRecords) then return (newRecords, g2) else computeBotRun params newRecords g2 genCap

recordBotRun :: Params -> BotRecords -> StdGen -> String -> IO ()
recordBotRun params records g recordType = do
    numGens <- if recordType == "default"
        then (return . length . bestFs) records
        else if recordType == "evo"
            then putStr "\nEnter the generation you would like to plot up to: " >> readLn >>= (\x -> return (x + 1))
            else putStr "\nEnter desired generation of bot: " >> readLn >>= (\x -> return (x + 1))
    let truncRecords = getTruncRecords records numGens
        bot          = if recordType == "max" then last (maxBs truncRecords) else bestB truncRecords
    writeDataFile params truncRecords bot
    callCommand $ unwords ["python src\\PlotBotData.py", "Bot_" ++ show params, recordType, show (numGens - 1)]
    if recordType == "default"
        then do
            writeFile (".stack-work\\runs\\Bot_" ++ show params ++ ".txt") (show (records, unStdGen g))
            putStrLn "Plots drawn, save file overwritten."
        else do
            putStrLn "\nPlots drawn."

getTruncRecords :: BotRecords -> Int -> BotRecords
getTruncRecords records numGens =
    let numRecordBreaks = (countRecordBreaks . take numGens . BotGA.maxFs) records
        newMaxBs        = take numRecordBreaks (maxBs records)
        newMaxFs        = take numGens (BotGA.maxFs records)
        newBestFs       = take numGens (bestFs records)
        newAvgFs        = take numGens (BotGA.avgFs records)
    in  BotRecords newMaxBs newMaxFs (bestB records) newBestFs newAvgFs (legss records) (fitss records)

writeDataFile :: Params -> BotRecords -> Bot -> IO ()
writeDataFile params records bot = do
    let legMoves         = testBot (i params) bot
        unzippedLegMoves = map (fromTup . unzip) legMoves
        botPath          = getBotPath (i params) bot
        unzippedBotPath  = fromTup (unzip botPath)
        fitsTxt          = (unlines . map show) [BotGA.maxFs records, bestFs records, BotGA.avgFs records]
    writeFile ".stack-work\\datafile.txt" $ fitsTxt ++ show unzippedLegMoves ++ "\n" ++ show unzippedBotPath

---------------------------------------- LEG MAIN ----------------------------------------

newLegMain :: IO ()
newLegMain = do
    params <- getParams Nothing
    putStr "\nInitializing generation 0..."
    let (records, g) = runRand (startNetGA params) (mkStdGen $ NetGA.s params)
        !_           = last (NetGA.maxFs records)
    putStrLn "Success!"
    viewLeg params records g

loadLegMain :: IO ()
loadLegMain = do
    params <- getParams Nothing
    putStr "\nLoading save data..."
    recordStr <- readFile (".stack-work\\runs\\" ++ show params ++ ".txt")
    let (records, smgen) = read recordStr :: (NetRecords, SMGen)
        !_               = last (NetGA.maxFs records)
        g                = StdGen smgen
    putStrLn "Success!"
    viewLeg params records g

viewLeg :: Params -> NetRecords -> StdGen -> IO ()
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

loopLeg :: Params -> NetRecords -> StdGen -> IO ()
loopLeg params records g = do
    putStr $ "\nComputing generation " ++ (show . length . NetGA.maxFs) records ++ "..."
    let (newRecords, g2) = runRand (runNetGA params records) g
        !_               = last (NetGA.maxFs newRecords)
    putStrLn "Success!"
    recordLegRun params newRecords g2 False
    loopLeg params newRecords g2

recordLegRun :: Params -> NetRecords -> StdGen -> Bool -> IO ()
recordLegRun params records g isFinal = do
    let label        = show params
        legPosits    = testNet (NetGA.i params) $ last (maxNs records)
        (xLst, yLst) = unzip legPosits
    writeFile ".stack-work\\datafile.txt"
        $  show (NetGA.maxFs records)
        ++ "\n"
        ++ show (NetGA.avgFs records)
        ++ "\n"
        ++ show xLst
        ++ "\n"
        ++ show yLst
    callCommand $ "python src\\PlotLegData.py " ++ label ++ " " ++ show isFinal
    putStrLn "Plot successfully made!"
    writeFile (".stack-work\\runs\\" ++ label ++ ".txt") (show (records, unStdGen g))
    putStrLn "\nPlots drawn, run saved."

getParams :: Maybe Int -> IO Params
getParams currSeed = do
    let getIntLine str = do
            putStr str
            input <- getLine
            readIO input :: IO Int
        getFloatLine str = do
            putStr str
            input <- getLine
            readIO input :: IO Float
    putStrLn "\nEnter parameters:"
    numNets  <- getIntLine "numNets = "
    numNrns  <- getIntLine "numNrns = "
    numTests <- getIntLine "numTests = "
    iter     <- getIntLine "iter = "
    mut      <- getFloatLine "mut = "
    putStr "Turn elitism on? (y/n): "
    elitismInput <- getLine
    let elitism = elitismInput == "y"
    seed <- maybe (getIntLine "Initial RNG seed = ") return currSeed
    let params = Params numNets numNrns numTests iter mut elitism seed
    putStr "Are these values all correct? (y/n): "
    input <- getLine
    let action
            | input == "y" = return params
            | input == "n" = getParams currSeed
            | otherwise = do
                putStr "\nInvalid input. Try again."
                getParams currSeed
    action
