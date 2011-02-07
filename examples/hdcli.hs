{-# LANGUAGE
    RecordWildCards #-}
module Main(main) where

import Hdis86

import System.Environment ( getArgs )
import Control.Monad
import Text.Printf
import qualified Data.ByteString as BS

data Opts = Opts
  { skip   :: Int
  , count  :: Maybe Int
  , doOff  :: Bool
  , doHex  :: Bool
  , doDump :: Bool
  , file   :: Maybe String
  } deriving (Show)

getOpts :: [String] -> (Opts, Config)
getOpts = go (dfl, amd32 { cfgSyntax = SyntaxIntel }) where
  dfl = Opts 0 Nothing True True False Nothing
  go pair@(opt,cfg) = f where
    f []                      = pair
    f [fname@(c:_)] | c /= '-' = (opt { file = Just fname }, cfg)
    f ("-16"       :xs) = go (opt, cfg { cfgWordSize = Bits16      }) xs
    f ("-32"       :xs) = go (opt, cfg { cfgWordSize = Bits32      }) xs
    f ("-64"       :xs) = go (opt, cfg { cfgWordSize = Bits64      }) xs
    f ("-intel"    :xs) = go (opt, cfg { cfgSyntax   = SyntaxIntel }) xs
    f ("-att"      :xs) = go (opt, cfg { cfgSyntax   = SyntaxATT   }) xs
    f ("-v":"intel":xs) = go (opt, cfg { cfgVendor   = Intel       }) xs
    f ("-v":"amd"  :xs) = go (opt, cfg { cfgVendor   = AMD         }) xs
    f ("-o":n      :xs) = go (opt, cfg { cfgOrigin   = parse n     }) xs
    f ("-s":n      :xs) = go (opt { skip   = parse n        }, cfg  ) xs
    f ("-c":n      :xs) = go (opt { count  = Just $ parse n }, cfg  ) xs
    f ("-noff"     :xs) = go (opt { doOff  = False          }, cfg  ) xs
    f ("-nohex"    :xs) = go (opt { doHex  = False          }, cfg  ) xs
    f ("-dump"     :xs) = go (opt { doDump = True           }, cfg  ) xs
    f ("-h"        :_ ) = error "takes the same arguments as udcli"
    f xs = error ("could not parse options: " ++ show xs)

  parse :: (Read a) => String -> a
  parse xs = case reads xs of
    [(v,"")] -> v
    _ -> error ("could not parse: " ++ show xs)

main :: IO ()
main = do
  (opts@Opts{..}, cfg) <- getOpts `fmap` getArgs
  inpt <- case file of
    Nothing -> BS.getContents
    Just f  -> BS.readFile f
  let chunk = maybe id BS.take count . BS.drop skip $ inpt
  let inst = disassembleMetadata cfg chunk
  mapM_ (out opts) inst

out :: Opts -> Metadata -> IO ()
out Opts{..} Metadata{..} = do
  when doOff $ printf "%016x " mdOffset
  let (hex, leftover) = splitAt 16 mdHex
  when doHex $ printf "%-16s" hex
  _ <- printf " %-24s" mdAssembly
  when (doHex && leftover /= "") $ do
    putChar '\n'
    when doOff $ putStr "                -"
    printf "%-16s" leftover
  when doDump (putStr ('\n' : replicate 34 ' ') >> putStr (show mdInst) >> putChar '\n')
  putChar '\n'
