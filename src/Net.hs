module Net where

import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                )
import           Nrn                            ( Nrn
                                                , makeRandNrns
                                                )
import           Util                           ( iterateR )

data Net = Net
    { getOutNrn1 :: Nrn
    , getOutNrn2 :: Nrn
    , getHidNrns :: [Nrn]
    }
    deriving (Eq, Show, Read)

getNrns :: Net -> [Nrn]
getNrns (Net outNrn1 outNrn2 hidNrns) = outNrn1 : outNrn2 : hidNrns

makeNet :: [Nrn] -> Net
makeNet nrns = Net (head nrns) (nrns !! 1) (drop 2 nrns)

-- pluralized version of below function
makeRandNets :: Int -> Int -> Rand StdGen [Net]
makeRandNets numNrns = iterateR (makeRandNet numNrns)

-- generates a neural network with randomly-generated neurons
makeRandNet :: Int -> Rand StdGen Net
makeRandNet numNrns = do
    hidNrns <- makeRandNrns numNrns True (numNrns - 2)
    outNrns <- makeRandNrns numNrns False 2
    return (Net (head outNrns) (last outNrns) hidNrns)
