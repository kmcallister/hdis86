-- | Incremental pure disassembly.

module Hdis86.Incremental
  (
    disassembleOne
  , disassembleLazy
  ) where

import Hdis86.Types
import qualified Hdis86.Pure as P
import qualified Hdis86.IO   as I

import Data.Maybe

import System.IO.Unsafe ( unsafePerformIO )

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

disOne :: Config -> BS.ByteString -> Maybe Metadata
disOne cfg bs = unsafePerformIO $ do
  ud <- I.newUD
  I.setInputBuffer ud bs
  I.setConfig      ud cfg
  n <- I.advance ud
  case n of
    Just _  -> Just `fmap` I.getMetadata ud
    Nothing -> return Nothing

-- | Split a @'BS.ByteString'@ into an instruction and the remaining
-- @'BS.ByteString'@.
--
-- Returns @'Nothing'@ if the input is empty or contains an
-- incomplete instruction.
disassembleOne :: Config -> BS.ByteString -> Maybe (Metadata, BS.ByteString)
disassembleOne cfg bs = disOne cfg bs >>= (fmap getRem . getMD) where
  -- invalid instruction: distinguish from incomplete instruction
  -- by appending 0x90 = nop
  getMD m@Metadata { mdInst  = i0@Inst { inOpcode = Iinvalid },
                     mdBytes = ibs }
    = case P.disassemble cfg (ibs `BS.snoc` 0x90) of
        [i1, Inst [] Inop []] | i0 == i1  -> Just m
        _ -> Nothing

  getMD  m = Just m

  getRem m = (m, BS.drop (fromIntegral $ mdLength m) bs)

-- | Disassemble a lazy @'BL.ByteString'@.
--
-- The output is produced lazily.
disassembleLazy :: Config -> BL.ByteString -> [Metadata]
disassembleLazy cfg = go (cfgOrigin cfg) BS.empty . BL.toChunks where
  go n bs chunks = let newcfg = cfg { cfgOrigin = n } in
    case disassembleOne newcfg bs of
      Just (md, bsrem) -> md : go nn bsrem chunks where
        nn = n + fromIntegral (mdLength md)

      Nothing -> case chunks of
        (x:xs) -> go n (bs `BS.append` x) xs
        -- end of input; produce a final incomplete instruction if any
        []     -> maybeToList $ disOne newcfg bs
