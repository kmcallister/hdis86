{-# LANGUAGE
    DeriveDataTypeable
  , RecordWildCards #-}

-- | Interface to the `udis86` disassembler.
--
-- This is the simplest, most high-level interface.
--
-- See @'Udis86.IO'@ if you need more control or performance.
module Udis86.Pure
  ( -- * Simple disassembly
    disassemble

    -- * Disassembly with full metadata
  , disassembleMetadata
  , Metadata(..)

    -- * Configuration
  , Config(..)

    -- * Common configurations
  , intel32, intel64, amd32, amd64
  ) where

import Udis86.Types
import Udis86.IO ( UD )
import qualified Udis86.IO as I

import Data.Typeable ( Typeable )
import Data.Data     ( Data )

import Control.Applicative
import Data.Word

import System.IO.Unsafe ( unsafePerformIO )

import qualified Data.ByteString as BS

-- | Configuration of the disassembler
data Config = Config
  { cfgVendor   :: Vendor    -- ^ CPU vendor; determines the instruction set used
  , cfgWordSize :: WordSize  -- ^ Disassemble 16-, 32-, or 64-bit code
  , cfgSyntax   :: Syntax    -- ^ Syntax to use when generating assembly
  , cfgOrigin   :: Word64    -- ^ Address where the first instruction would live in memory
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

intel32, intel64, amd32, amd64 :: Config
intel32 = Config Intel Bits32 SyntaxNone 0
intel64 = Config Intel Bits64 SyntaxNone 0
amd32   = Config AMD   Bits32 SyntaxNone 0
amd64   = Config AMD   Bits64 SyntaxNone 0

runWith :: (UD -> IO a) -> UD -> IO [a]
runWith get s = go where
  go = do
    n <- I.disassemble s
    if n > 0
      then liftA2 (:) (get s) go
      else return []

disWith :: (UD -> IO a) -> Config -> BS.ByteString -> [a]
disWith get Config{..} bs = unsafePerformIO $ do
  ud <- I.newUD
  I.setInputBuffer ud bs
  I.setVendor   ud cfgVendor
  I.setWordSize ud cfgWordSize
  I.setSyntax   ud cfgSyntax
  I.setIP       ud cfgOrigin
  runWith get ud

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
