module Bot where

import           Control.Monad.Random           ( Rand
                                                , StdGen
                                                )
import           Net                            ( Net
                                                , chrom2Net
                                                , makeRandNets
                                                , net2Chrom
                                                )
import           Util                           ( iterateR )

newtype Bot = Bot {getNets :: [Net]} deriving (Eq, Show, Read)

--pluralized version of below function
makeRandBots :: Int -> Int -> Rand StdGen [Bot]
makeRandBots numNrns = iterateR (makeRandBot numNrns)

-- generates a bot with randomly-generated neural networks
makeRandBot :: Int -> Rand StdGen Bot
makeRandBot numNrns = do
    nets <- makeRandNets numNrns 6
    return (Bot nets)

-- converts a bot to a chromosome
bot2Chrom :: Bot -> Maybe [[[String]]]
bot2Chrom = mapM net2Chrom . getNets

-- converts a chromosome to a bot
chrom2Bot :: [[[String]]] -> Maybe Bot
chrom2Bot chrom = mapM chrom2Net chrom >>= (Just . Bot)
