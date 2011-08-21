module Main(main) where

{- Check correctness and speed of lazy ByteString disassembly
   versus strict ByteString disassembly.

   $ for p in mwc-random clock groom; do cabal install $p; done
   $ ghc --make -O test_lazy_bytestring.hs
   $ ./test_lazy_bytestring
-}

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed  as V
import qualified System.Random.MWC    as R
import qualified System.Posix.Clock   as C

import Control.Monad
import Control.Exception ( evaluate )
import System.Exit
import System.IO
import Text.Groom
import Text.Printf

import Hdis86
import Hdis86.Incremental

splitBS :: R.GenIO -> BS.ByteString -> IO [BS.ByteString]
splitBS _   bs | BS.length bs < 2 = return [bs]
splitBS gen bs = do
    die <- R.uniformR (0,4) gen
    if die == (0 :: Int)
        then return [bs]
        else do
            i <- R.uniformR (1, BS.length bs - 1) gen
            let (x,y) = BS.splitAt i bs
            fmap (x:) (splitBS gen y)

time :: IO () -> IO Double
time x = do
    let get = C.getTime C.Monotonic
    C.TimeSpec s0 ns0 <- get
    x
    C.TimeSpec s1 ns1 <- get
    let scale = ((10 ** (-9)) *) . fromIntegral
    return (fromIntegral (s1 - s0) + scale (ns1 - ns0))

main :: IO ()
main = R.withSystemRandom $ \gen -> forever $ do
    size   <- R.uniformR (2^16, 2^21) gen
    vec    <- R.uniformVector gen size
    let bs =  BS.pack (V.toList vec)
    chunks <- splitBS gen bs
    let bl =  BL.fromChunks chunks
    _      <- evaluate (BS.length bs)
    _      <- evaluate (BL.length bl)

    let ms = disassembleMetadata amd64 bs
        ml = disassembleLazy     amd64 bl
        force = time . mapM_ (evaluate . inOpcode . mdInst)
    ts <- force $ disassembleMetadata amd64 bs
    tl <- force $ disassembleLazy     amd64 bl

    _  <- printf "%4d kB in %3d chunks: %5.2f s / %5.2f s = %6.2f%% speed\n"
        (size `div` 1024) (length chunks) ts tl (100 * ts / tl)
    when (ms /= ml) $ do
        hPutStr stderr "FAIL; check out.*\n"
        writeFile "out.strict" $ groom ms
        writeFile "out.lazy"   $ groom ml
        exitWith (ExitFailure 1)
