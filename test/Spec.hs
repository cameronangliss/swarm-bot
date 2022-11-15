module Spec where

import           BotGA                          ( BotRecords(maxFs)
                                                , getInitBotRecords
                                                , runBotGA
                                                )
import           Control.Monad.Random           ( mkStdGen
                                                , runRand
                                                )
import           NetGA                          ( Params(..) )

main :: IO ()
main = do
    let params = Params { numNets = 100, numNrns = 5, numTests = 1, i = 100, mut = 1.0e-3, elitism = True, s = 1 }
        (records, g) = runRand (getInitBotRecords params) (mkStdGen $ s params)
        (nextRecords, _) = runRand (runBotGA params records) g
    print (last $ maxFs nextRecords)
