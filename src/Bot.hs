module Bot where

import           Control.Monad.Random (Rand, StdGen)
import           Net                  (Net, makeRandNets)
import           Util                 (iterateR)

newtype Bot = Bot {getNets :: [Net]} deriving (Eq, Show, Read)

--pluralized version of below function
makeRandBots :: Int -> Int -> Rand StdGen [Bot]
makeRandBots numNrns = iterateR (makeRandBot numNrns)

-- generates a bot with randomly-generated neural networks
makeRandBot :: Int -> Rand StdGen Bot
makeRandBot numNrns = do
    nets <- makeRandNets numNrns 6
    return (Bot nets)
