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
    params <- getParams Nothing
    putStr "\nInitializing generation 0..."
    let (records, g) = runRand (getInitBotRecords params) (mkStdGen $ s params)
        !maxFit      = last (bestFs records)
    putStrLn "Success!"
    viewBot params records g

loadBotMain :: IO ()
loadBotMain = do
    params <- getParams Nothing
    putStr "\nLoading save data..."
    recordStr <- readFile (".stack-work\\runs\\Bot_" ++ show params ++ ".txt")
    let (records, smgen) = read recordStr :: (BotRecords, SMGen)
        g                = StdGen smgen
        !maxFit          = last (bestFs records)
    putStrLn "Success!"
    viewBot params records g

viewBot :: Params -> BotRecords -> StdGen -> IO ()
viewBot params records g = do
    putStrLn
        $  "\nCompleted "
        ++ (show . subtract 1 . length . bestFs) records
        ++ " generations. What would you like to do?"
    putStrLn "Options:"
    putStrLn "    bestBot  -> print bot with greatest fitness from run"
    putStrLn "    bestFit  -> print greatest fitness from run"
    putStrLn "    bestFits -> print list of reference fitnesses from run"
    putStrLn "    avgFits  -> print average fitness over all legs from each generation"
    putStrLn "    plotEvo  -> plots unaltered data from run up to desired generation"
    putStrLn "    plotBest -> plots movement of maxBot from desired generation"
    putStrLn "    run      -> resume run with unchanged parameters up to desired generation"
    putStrLn "    change   -> change parameters of run"
    putStrLn "    back     -> return to main menu"
    input <- getLine
    let action
            | input == "bestBot" = do
                putStrLn ""
                (print . last . bestBs) records
                viewBot params records g
            | input == "bestFit" = do
                putStrLn ""
                (print . round . last . bestFs) records
                viewBot params records g
            | input == "bestFits" = do
                putStrLn ""
                (print . map round . bestFs) records
                viewBot params records g
            | input == "avgFits" = do
                putStrLn ""
                (print . map round . BotGA.avgFs) records
                viewBot params records g
            | input == "plotEvo" = do
                recordBotRun params records g "evo"
                viewBot params records g
            | input == "plotBest" = do
                recordBotRun params records g "best"
                viewBot params records g
            | input == "run" = do
                putStr "\nEnter desired generation to compute up to: "
                input <- readLn
                if input < length (bestFs records) - 1
                    then viewBot params records g
                    else do
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
    putStr $ "\nComputing up to generation " ++ show (length $ bestFs records) ++ "..."
    let (newRecords, g2) = runRand (runBotGA params records) g
        !maxFit          = last (bestFs newRecords)
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
    let truncRecords = getTruncRecords params records numGens
        bot          = last (bestBs truncRecords)
    writeDataFile params truncRecords bot
    callCommand $ unwords ["python src\\PlotBotData.py", "Bot_" ++ show params, recordType, show (numGens - 1)]
    if recordType == "default"
        then do
            writeFile (".stack-work\\runs\\Bot_" ++ show params ++ ".txt") (show (records, unStdGen g))
            putStrLn "\nPlots drawn, save file overwritten."
        else do
            putStrLn "\nPlots drawn."

getTruncRecords :: Params -> BotRecords -> Int -> BotRecords
getTruncRecords params records numGens =
    let numRecordBreaks = (countRecordBreaks . take numGens . bestFs) records
        newBestBs       = take numRecordBreaks (bestBs records)
        newBestFs       = take numGens (bestFs records)
        newAvgFs        = take numGens (BotGA.avgFs records)
    in  BotRecords newBestBs newBestFs newAvgFs (legss records) (fitss records)

writeDataFile :: Params -> BotRecords -> Bot -> IO ()
writeDataFile params records bot = do
    let legMoves         = testBot (i params) bot
        unzippedLegMoves = map (fromTup . unzip) legMoves
        botPath          = getBotPath (i params) bot
        unzippedBotPath  = fromTup (unzip botPath)
        fitsTxt          = (unlines . map show) [bestFs records, BotGA.avgFs records]
    writeFile ".stack-work\\datafile.txt" $ fitsTxt ++ show unzippedLegMoves ++ "\n" ++ show unzippedBotPath

---------------------------------------- LEG MAIN ----------------------------------------

newLegMain :: IO ()
newLegMain = do
    params <- getParams Nothing
    putStr "\nInitializing generation 0..."
    let (records, g) = runRand (startNetGA params) (mkStdGen $ NetGA.s params)
        !maxFit      = last (NetGA.maxFs records)
    putStrLn "Success!"
    viewLeg params records g

loadLegMain :: IO ()
loadLegMain = do
    params <- getParams Nothing
    putStr "\nLoading save data..."
    recordStr <- readFile (".stack-work\\runs\\" ++ show params ++ ".txt")
    let (records, smgen) = read recordStr :: (NetRecords, SMGen)
        g                = StdGen smgen
        !maxFit          = last (maxNs records)
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
        !maxFit          = last (NetGA.maxFs newRecords)
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
    numNets <- getIntLine "numNets = "
    numNrns <- getIntLine "numNrns = "
    iter    <- getIntLine "iter = "
    mut     <- getFloatLine "mut = "
    putStr "Turn elitism on? (y/n): "
    elitismInput <- getLine
    let elitism = elitismInput == "y"
    seed <- maybe (getIntLine "Initial RNG seed = ") return currSeed
    let params = Params numNets numNrns iter mut elitism seed
    putStr "Are these values all correct? (y/n): "
    input <- getLine
    let action
            | input == "y" = return params
            | input == "n" = getParams currSeed
            | otherwise = do
                putStr "\nInvalid input. Try again."
                getParams currSeed
    action
