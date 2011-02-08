{-# LANGUAGE
    DeriveDataTypeable #-}

-- | Interface to the @udis86@ disassembler.
--
-- This is the simplest, most high-level interface.
--
-- See @'Hdis86.IO'@ if you need more control or performance.
module Hdis86.Pure
  ( -- * Simple disassembly
    disassemble

    -- * Disassembly with full metadata
  , disassembleMetadata
  , Metadata(..)
  ) where

import Hdis86.Types
import Hdis86.IO ( UD )
import qualified Hdis86.IO as I

import Data.Typeable ( Typeable )
import Data.Data     ( Data )

import Control.Applicative
import Data.Word
import Data.Function

import System.IO.Unsafe ( unsafePerformIO )

import qualified Data.ByteString as BS

disWith :: (UD -> IO a) -> Config -> BS.ByteString -> [a]
disWith get cfg bs = unsafePerformIO $ do
  ud <- I.newUD
  I.setInputBuffer ud bs
  I.setConfig      ud cfg
  fix $ \loop -> do
    n <- I.disassemble ud
    if n > 0
      then liftA2 (:) (get ud) loop
      else return []

-- | Disassemble machine code.
disassemble :: Config -> BS.ByteString -> [Instruction]
disassemble = disWith I.getInstruction

-- | An instruction with full metadata.
data Metadata = Metadata
  { mdOffset   :: Word64         -- ^ Offset of the start of this instruction
  , mdLength   :: Word           -- ^ Length of this instruction in bytes
  , mdHex      :: String         -- ^ Hexadecimal representation of this instruction
  , mdBytes    :: BS.ByteString  -- ^ Bytes that make up this instruction
  , mdAssembly :: String         -- ^ Assembly code for this instruction
  , mdInst     :: Instruction    -- ^ The instruction itself
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | Disassemble machine code, with full metadata.
disassembleMetadata :: Config -> BS.ByteString -> [Metadata]
disassembleMetadata = disWith $ \ud -> Metadata
  <$> I.getOffset      ud
  <*> I.getLength      ud
  <*> I.getHex         ud
  <*> I.getBytes       ud
  <*> I.getAssembly    ud
  <*> I.getInstruction ud
