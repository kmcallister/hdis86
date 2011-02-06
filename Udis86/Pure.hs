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
  , Config(..), Vendor(..), Syntax(..)

    -- * Common configurations
  , intel32, intel64, amd32, amd64
  ) where

import Udis86.Types
import Udis86.IO ( UD )
import qualified Udis86.IO as I

import Data.Typeable ( Typeable )
import Data.Data     ( Data )

import Control.Monad
import Control.Applicative
import Data.Word

import System.IO.Unsafe ( unsafePerformIO )

import qualified Data.ByteString as BS

-- | Configuration of the disassembler
data Config = Config
  { cfgVendor   :: Vendor    -- ^ CPU vendor
  , cfgWordSize :: WordSize  -- ^ Disassemble 16-, 32-, or 64-bit code
  , cfgSyntax   :: Syntax    -- ^ When generating assembly, use this syntax
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | CPU vendors and their associated instruction-set variations.
data Vendor
  = Intel
  | AMD
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

-- | Selection of assembly output syntax.
data Syntax
  = SyntaxNone   -- ^ Don't generate assembly syntax
  | SyntaxIntel  -- ^ Intel- / NASM-like syntax
  | SyntaxATT    -- ^ AT&T- / @gas@-like syntax
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

intel32, intel64, amd32, amd64 :: Config
intel32 = Config Intel Bits32 SyntaxNone
intel64 = Config Intel Bits64 SyntaxNone
amd32   = Config AMD   Bits32 SyntaxNone
amd64   = Config AMD   Bits64 SyntaxNone

runWith :: (UD -> IO a) -> UD -> IO [a]
runWith get s = go where
  go = do
    n <- I.disassemble s
    if n > 0
      then liftM2 (:) (get s) go
      else return []

disWith :: (UD -> IO a) -> Config -> BS.ByteString -> [a]
disWith get Config{..} bs = unsafePerformIO $ do
  ud <- I.newUD
  I.setInputBuffer ud bs
  case cfgVendor of
    Intel -> I.setVendorIntel ud
    AMD   -> I.setVendorAMD   ud
  I.setWordSize ud cfgWordSize
  case cfgSyntax of
    SyntaxNone  -> I.setSyntaxNone  ud
    SyntaxIntel -> I.setSyntaxIntel ud
    SyntaxATT   -> I.setSyntaxATT   ud
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
  , mdAssembly :: String         -- ^ Assembly syntax for this instruction
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
