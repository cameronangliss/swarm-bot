module Chromable where

import           Bot                            ( Bot(Bot)
                                                , getNets
                                                )
import           Net                            ( Net
                                                , getNrns
                                                , makeNet
                                                )
import           Nrn                            ( Nrn(Nrn)
                                                , fromBin
                                                , getNumBits
                                                , toBin
                                                )
import           Util                           ( chunksOf )

class Chromable a c | a -> c where
    toChrom :: a -> c
    fromChrom :: c -> a

instance Chromable Nrn [String] where
    toChrom (Nrn v t s ws iWs) =
        let vC   = toBin 'v' v
            tC   = toBin 't' t
            sC   = toBin 's' s
            wCs  = concatMap (toBin 'w') ws
            iWCs = concatMap (toBin 'w') iWs
        in  [vC, tC, sC, wCs, iWCs]
    fromChrom chrom =
        let v   = fromBin 'v' (head chrom)
            t   = fromBin 't' (chrom !! 1)
            s   = fromBin 's' (chrom !! 2)
            ws  = map (fromBin 'w') $ chunksOf (getNumBits 'w') (chrom !! 3)
            iWs = map (fromBin 'w') $ chunksOf (getNumBits 'w') (chrom !! 4)
        in  Nrn v t s ws iWs

instance Chromable Net [[String]] where
    toChrom   = map toChrom . getNrns
    fromChrom = makeNet . map fromChrom

instance Chromable Bot [[[String]]] where
    toChrom   = map toChrom . getNets
    fromChrom = Bot . map fromChrom
